module Pnums

# 000 -> [0, 0]
# 001 -> (0, 1)
# 010 -> [1, 1]
# 011 -> (1, /0)
# 100 -> [/0, /0]
# 101 -> (/0, -1)
# 110 -> [-1, -1]
# 111 -> (-1, 0)

const exacts = [-1//1, 0//1, 1//1]
const pnnvalues = UInt8(2*(length(exacts) + 1))
const pnmask = UInt8(pnnvalues - 0x01) # "00000111"

# Pack a Pnum into the 3 trailing bits of a UInt8
# TODO am I going to get hurt by endianness here?
immutable Pnum
  v::UInt8
  # TODO, don't make this constructor part of the public interface. It's
  # kind of dangerous because it just masks off a bunch of bits. I think
  # I want to move to having the Pnum constructor call "convert", and
  # requiring use of a "Bitmask" to do raw construction.
  Pnum(v) = new(UInt8(v) & pnmask)
end

const pnzero = Pnum(0x00)
const pninf = Pnum(pnnvalues >> 1)

Base.zero(::Type{Pnum}) = pnzero
iszero(x::Pnum) = x.v == pnzero.v
Base.isinf(x::Pnum) = x.v == pninf.v
isexact(x::Pnum) = trailing_ones(x.v) == 0 # Check the ubit
# Next and prev move us anti-clockwise or clockwise around the
# projective circle
next(x::Pnum) = Pnum(x.v + one(x.v))
prev(x::Pnum) = Pnum(x.v - one(x.v))

isstrictlynegative(x::Pnum) = x.v > pninf.v

function exactvalue(x::Pnum)
  if isinf(x)
    1//0
  else
    exacts[mod((x.v >> 1) + 0x02, pnnvalues >> 1)]
  end
end

function Base.convert(::Type{Pnum}, x::Real)
  isinf(x) && return pninf
  r = searchsorted(exacts, x)
  if first(r) == last(r)
    return Pnum(mod(UInt8(first(r) << 1) - (pnnvalues >> 1), pnnvalues))
  elseif first(r) > length(exacts)
    return prev(pninf)
  elseif last(r) == 0
    return next(pninf)
  else
    return next(Pnum(mod(UInt8(last(r) << 1) - (pnnvalues >> 1), pnnvalues)))
  end
end

Base.(:-)(x::Pnum) = Pnum(-x.v)
# Negate and rotate 180 degrees
recip(x::Pnum) = Pnum(-x.v - pninf.v)

# Calling these slowplus and slowtimes because, in a final
# implementation, they will probably be used to generate lookup tables,
# and the lookup tables will be used for runtime arithmetic

function _exactplus(x::Pnum, y::Pnum)
  isinf(x) || isinf(y) ? pninf : convert(Pnum, exactvalue(x) + exactvalue(y))
end

# Note, returns a Pbound
function slowplus(x::Pnum, y::Pnum)
  (isinf(x) && isinf(y)) && return pbeverything

  xexact, yexact = isexact(x), isexact(y)
  bothexact = xexact && yexact

  x1, x2 = xexact ? (x, x) : (prev(x), next(x))
  y1, y2 = yexact ? (y, y) : (prev(y), next(y))

  z1 = _exactplus(x1, y1)
  z2 = _exactplus(x2, y2)

  z1 = !bothexact && isexact(z1) ? next(z1) : z1
  z2 = !bothexact && isexact(z2) ? prev(z2) : z2

  Pbound(z1, z2)
end

function _exacttimes(x::Pnum, y::Pnum)
  isinf(x) || isinf(y) ? pninf : convert(Pnum, exactvalue(x)*exactvalue(y))
end

# Note, returns a Pbound
function slowtimes(x::Pnum, y::Pnum)
  (isinf(x) && iszero(y)) && return pbeverything
  (iszero(x) && isinf(y)) && return pbeverything

  xexact, yexact = isexact(x), isexact(y)
  bothexact = xexact && yexact

  x1, x2 = xexact ? (x, x) : (prev(x), next(x))
  y1, y2 = yexact ? (y, y) : (prev(y), next(y))

  if (isstrictlynegative(y))
    x1, x2 = x2, x1
  end

  if (isstrictlynegative(x))
    y1, y2 = y2, y1
  end

  z1 = _exacttimes(x1, y1)
  z2 = _exacttimes(x2, y2)

  z1 = !bothexact && isexact(z1) ? next(z1) : z1
  z2 = !bothexact && isexact(z2) ? prev(z2) : z2

  Pbound(z1, z2)
end

# TODO plan to replace these with lut operations at some point (maybe)
Base.(:+)(x::Pnum, y::Pnum) = slowplus(x, y)
Base.(:-)(x::Pnum, y::Pnum) = slowplus(x, -y)
Base.(:*)(x::Pnum, y::Pnum) = slowtimes(x, y)
Base.(:/)(x::Pnum, y::Pnum) = slowtimes(x, recip(y))

# A Pbound is stored as a packed binary UInt, consisting of a leading
# two bit tag followed by 2 Pnums.
#
# Tag meanings:
# "10": empty set, regardless of the following bits.
# "00": anti-clockwise interval from first Pnum to second
# "11": resserved, currently illegal
# "01": reserved, currently illegal
immutable Pbound
  v::UInt8
end

const pbshiftsize = 4*sizeof(Pbound) - 1

Pbound(x::Pnum, y::Pnum) = Pbound((x.v << pbshiftsize) | y.v)
unpack(x::Pbound) = (
  isempty(x),
  Pnum(x.v >> pbshiftsize),
  Pnum(x.v)
)

isempty(x::Pbound) = leading_zeros(x.v) == 0 # checks top bit
function iseverything(x::Pbound)
  empty, x1, x2 = unpack(x)
  empty && return false
  mod(x1.v - x2.v, pnnvalues) == one(x.v)
end

# There are actually n^2 representations for "empty", and n
# representations for "everything", but these are the canonical ones.
const pbempty = Pbound(1 << (8*sizeof(Pbound) - 1)) # "10000000"
const pbeverything = Pbound(pnzero, prev(pnzero))
const pbzero = Pbound(pnzero, pnzero)

Base.zero(::Type{Pbound}) = pbzero

function Base.convert(::Type{Pbound}, x::Real)
  x1 = convert(Pnum, x)
  Pbound(x1, x1)
end

function Base.(:-)(x::Pbound)
  empty, x1, x2 = unpack(x)
  empty && return x
  Pbound(-x2, -x1)
end

function recip(x::Pbound)
  empty, x1, x2 = unpack(x)
  empty && return x
  Pbound(recip(x2), recip(x1))
end

function Base.complement(x::Pbound)
  empty, x1, x2 = unpack(x)
  empty && return pbeverything
  iseverything(x) && return pbempty
  Pbound(next(x2), prev(x1))
end

function Base.in(y::Pnum, x::Pbound)
  empty, x1, x2 = unpack(x)
  empty && return false
  y.v - x1.v <= x2.v - x1.v
end

# Arithmetic:
# Make tables for + and *. They will be 8x8 arrays of Pbounds.
# All arithmetic on "nothing" produces "nothing"
# Ways to produce "everything":
#   * /0 + /0
#   * 0*/0 or /0*0
#   * everything*(something except 0 or /0)
#   * everything + something
# Multiplying 0 or /0 by something (i.e. not nothing) produces 0 or /0

include("./io.jl")

export Pnum, Pbound, @pn_str, @pb_str, isexact, recip

end
