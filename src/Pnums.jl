# Pnum stands for either "prototype Unum," or "projective number." I
# haven't totally decided yet. I don't want to call these Unums yet
# because I've only implemented a tiny bit of the Unum 2.0 proposal,
# and I'm implementing some things that aren't in it (AFAICT).
#
# Pnums are exactly as described by Gustafson for Unums 2.0.
#
# Pbounds represent intervals of the stereographic circle (i.e. the
# projective real line). They are encoded as 2 Pnums, and you traverse
# them counter-clockwise from the first value to the second value.
#
# This means that there are n redundant representations of the entire
# set.
#
# The top two bits of a Pbound are a tag, which is set to 00 for normal
# Pbounds, and 10 for the empty set. The second bit of the tag is
# currently unused. When the empty set tag is present, the rest of the
# Pbound has no interpretation.
#
# One idea I had for the second tag bit is to allow a distinction
# between a completely empty set, the result of an operation over an
# interval that maps partially to the empty set. E.g. to encode the
# difference between sqrt(pb"(-1, 1)") and sqrt(pb"(0, 1)"). In the
# first case, part of the input maps to the real line, and part of it
# does not. In the second case, none of the input maps to the real
# line.

module Pnums

# 000 -> [0, 0]
# 001 -> (0, 1)
# 010 -> [1, 1]
# 011 -> (1, /0)
# 100 -> [/0, /0]
# 101 -> (/0, -1)
# 110 -> [-1, -1]
# 111 -> (-1, 0)

# Store unums in a byte with 5 leading zeros
# Store ubounds in a byte
# Store SOPNs in a byte
#
# Interesting that for these numbers, a ubound and a SOPN take the same
# number of bytes to represent
immutable Pnum
  v::UInt8
  # TODO, just throw an error if other bits are set. Normalizing here
  # seems dangerous. Probably also switch to bitmask indirection.
  Pnum(v) = new(UInt8(v) & 0x07) # TODO magic 00000111 bitmask
end

const pnzero = Pnum(0x00)
const pninf = Pnum(0x04)

zero(::Type{Pnum}) = pnzero
iszero(x::Pnum) = x.v == pnzero.v
Base.isinf(x::Pnum) = x.v == pninf.v
isexact(x::Pnum) = (x.v & 0x01) == 0x00 # Check the ubit
# Next and prev move us clockwise around the stereographic circle
next(x::Pnum) = Pnum(x.v + one(x.v))
prev(x::Pnum) = Pnum(x.v - one(x.v))

isstrictlynegative(x::Pnum) = x.v > pninf.v

const exacts = [-1//1, 0//1, 1//1]

function exactvalue(x::Pnum)
  if isinf(x)
    1//0
  else
    exacts[mod((x.v >> 1) + 2, 4)]
  end
end

function Base.convert(::Type{Pnum}, x::Real)
  isinf(x) && return pninf
  r = searchsorted(exacts, x)
  if first(r) == last(r)
    return Pnum(UInt8(mod(2*first(r) - 4, 8)))
  elseif first(r) > length(exacts)
    return prev(pninf)
  elseif last(r) == 0
    return next(pninf)
  else
    return next(Pnum(UInt8(mod(2*last(r) - 4, 8))))
  end
end

Base.(:-)(x::Pnum) = Pnum(-x.v)
# Negate and rotate 180 degrees
recip(x::Pnum) = Pnum(-x.v - 0x04)

# Calling these slowplus and slowtimes because, in a final
# implementation, they will probably be used to generate lookup tables,
# and the lookup tables will be used for runtime arithmetic

function _exactplus(x::Pnum, y::Pnum)
  isinf(x) || isinf(y) ? pninf : convert(Pnum, exactvalue(x) + exactvalue(y))
end

# Note, returns a Pbound
function slowplus(x::Pnum, y::Pnum)
  (isinf(x) && isinf(y)) && return everything

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
  (isinf(x) && iszero(y)) && return everything
  (iszero(x) && isinf(y)) && return everything

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

immutable Pbound
  v::UInt8
end

# TODO, 3 is a magic number (the number of bits in our Pnums)
Pbound(x::Pnum, y::Pnum) = Pbound((x.v << 3) | y.v)
unpack(x::Pbound) = (Pnum(x.v >> 3), Pnum(x.v))
# TODO, 0xc0 is a magic number: "11000000", the first two bits of a byte
tag(x::Pbound) = Pbound(x.v & 0xc0)

# TODO, 0x80 is "10000000", checks top bit
isempty(x::Pbound) = (x.v & 0x80) == 0x80
function iseverything(x::Pbound)
  x1, x2 = unpack(x)
  mod(x1.v - x2.v, 0x08) == one(x.v)
end

const empty = Pbound(0x80)
const everything = Pbound(zero(Pnum), prev(zero(Pnum)))

zero(::Type{Pbound}) = Pbound(zero(Pnum), zero(Pnum))

function Base.convert(::Type{Pbound}, x::Real)
  x1 = convert(Pnum, x)
  Pbound(x1, x1)
end

function Base.(:-)(x::Pbound)
  isempty(x) && return empty
  x1, x2 = unpack(x)
  Pbound(-x2, -x1)
end

function recip(x::Pbound)
  isempty(x) && return empty
  x1, x2 = unpack(x)
  Pbound(recip(x2), recip(x1))
end

function Base.complement(x::Pbound)
  isempty(x) && return everything
  iseverything(x) && return empty
  x1, x2 = unpack(x)
  Pbound(next(x2), prev(x1))
end

function Base.in(y::Pnum, x::Pbound)
  isempty(x) && return false
  x1, x2 = unpack(x)
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

immutable Sopn
  v::UInt8
end

include("./io.jl")

export Pnum, Pbound, @pn_str, @pb_str, isexact, recip

end
