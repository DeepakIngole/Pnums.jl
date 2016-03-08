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

# Used for indirection purposes to allow Pnum(x::Real) to do conversion
# while still having a way to create a Pnum from raw bits. Should I
# just use reinterpret for this purpose?
immutable Bitmask{T}
  v::T
end

# Pack a Pnum into the 3 trailing bits of a UInt8
# TODO am I going to get hurt by endianness here?
immutable Pnum <: Number
  v::UInt8
  Pnum(b::Bitmask{UInt8}) = new(b.v & pnmask)
end

rawpnum(v::UInt8) = Pnum(Bitmask(v))
Pnum(x::Real) = convert(Pnum, x)

const pnzero = rawpnum(0x00)
const pninf = rawpnum(pnnvalues >> 1)

Base.zero(::Type{Pnum}) = pnzero
iszero(x::Pnum) = x.v == pnzero.v
Base.isinf(x::Pnum) = x.v == pninf.v
isexact(x::Pnum) = trailing_ones(x.v) == 0 # Check the ubit
# Next and prev move us anti-clockwise or clockwise around the
# projective circle
next(x::Pnum) = rawpnum(x.v + one(x.v))
prev(x::Pnum) = rawpnum(x.v - one(x.v))

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
    return rawpnum(mod(UInt8(first(r) << 1) - (pnnvalues >> 1), pnnvalues))
  elseif first(r) > length(exacts)
    return prev(pninf)
  elseif last(r) == 0
    return next(pninf)
  else
    return next(rawpnum(mod(UInt8(last(r) << 1) - (pnnvalues >> 1), pnnvalues)))
  end
end

Base.(:-)(x::Pnum) = rawpnum(-x.v)
# Negate and rotate 180 degrees
recip(x::Pnum) = rawpnum(-x.v - pninf.v)

# Calling these slowplus and slowtimes because, in a final
# implementation, they will probably be used to generate lookup tables,
# and the lookup tables will be used for runtime arithmetic

function _exactplus(x::Pnum, y::Pnum)
  (isinf(x) || isinf(y)) ? pninf : convert(Pnum, exactvalue(x) + exactvalue(y))
end

# Note, returns a Pbound
function slowplus(x::Pnum, y::Pnum)
  (isinf(x) && isinf(y)) && return pbeverything
  (isinf(x) || isinf(y)) && return pbinf

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
  (isinf(x) || isinf(y)) ? pninf : convert(Pnum, exactvalue(x)*exactvalue(y))
end

# Note, returns a Pbound
function slowtimes(x::Pnum, y::Pnum)
  (isinf(x) && iszero(y)) && return pbeverything
  (iszero(x) && isinf(y)) && return pbeverything
  (isinf(x) || isinf(y)) && return pbinf
  (iszero(x) || iszero(y)) && return pbzero

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
Base.(:-)(x::Pnum, y::Pnum) = x + (-y)
Base.(:*)(x::Pnum, y::Pnum) = slowtimes(x, y)
Base.(:/)(x::Pnum, y::Pnum) = x*recip(y)

# Index midpoint between two Pnums. Note that this is asymmetric in
# the arguments: reversing them will return a point 180 degrees away.
function bisect(x::Pnum, y::Pnum)
  inc = mod(y.v - x.v, pnnvalues) >> one(x.v)
  rawpnum(mod(x.v + inc, pnnvalues))
end

# A Pbound is stored as a packed binary UInt, consisting of a leading
# two bit tag followed by 2 Pnums.
#
# Tag meanings:
# "10": empty set, regardless of the following bits.
# "00": anti-clockwise interval from first Pnum to second
# "11": resserved, currently illegal
# "01": reserved, currently illegal
immutable Pbound <: Number
  v::UInt8
  Pbound(b::Bitmask{UInt8}) = new(b.v)
end

rawpbound(v::UInt8) = Pbound(Bitmask(v))

function Base.convert(::Type{Pbound}, x::Real)
  x1 = convert(Pnum, x)
  Pbound(x1)
end

Pbound(x::Real) = convert(Pbound, x)

const pbshiftsize = 4*sizeof(Pbound) - 1

Pbound(x::Pnum, y::Pnum) = rawpbound((x.v << pbshiftsize) | y.v)

Base.convert(::Type{Pbound}, x::Pnum) = Pbound(x, x)
Pbound(x::Pnum) = Pbound(x, x)

unpack(x::Pbound) = (
  isempty(x),
  rawpnum(x.v >> pbshiftsize),
  rawpnum(x.v)
)

isempty(x::Pbound) = leading_zeros(x.v) == 0 # checks top bit

function iseverything(x::Pbound)
  empty, x1, x2 = unpack(x)
  empty && return false
  mod(x1.v - x2.v, pnnvalues) == one(x.v)
end

function isexact(x::Pbound)
  empty, x1, x2 = unpack(x)
  empty && return false
  x1.v == x2.v && isexact(x1)
end

function issinglepnum(x::Pbound)
  empty, x1, x2 = unpack(x)
  empty && return false
  x1.v == x2.v
end

# There are actually n^2 representations for "empty", and n
# representations for "everything", but these are the canonical ones.
const pbempty = rawpbound(UInt8(1 << (8*sizeof(Pbound) - 1))) # "10000000"
const pbeverything = Pbound(pnzero, prev(pnzero))
const pbzero = Pbound(pnzero)
const pbinf = Pbound(pninf)
const pbfinite = Pbound(next(pninf), prev(pninf))
const pbnonzero = Pbound(next(pnzero), prev(pnzero))
const pbneg = Pbound(next(pninf), prev(pnzero))
const pbpos = Pbound(next(pnzero), prev(pninf))

Base.zero(::Type{Pbound}) = pbzero

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

function Base.intersect(x::Pbound, y::Pbound)
  xempty, x1, x2 = unpack(x)
  yempty, y1, y2 = unpack(y)
  (xempty || yempty) && return (pbempty, pbempty)
  iseverything(x) && return (y, pbempty)
  iseverything(y) && return (x, pbempty)
  x == y && return (x, pbempty)

  # x and y cover the entire projective circle, but are not equal
  # thus they have 2 intersections
  x1 in y && x2 in y && y1 in x && y2 in x && return (Pbound(y1, x2), Pbound(x1, y2))
  # One bound covers the other
  x1 in y && x2 in y && return (x, pbempty)
  y1 in x && y2 in x && return (y, pbempty)
  # Bounds overlap
  x1 in y && return (Pbound(x1, y2), pbempty)
  x2 in y && return (Pbound(y1, x2), pbempty)
  # Bounds are disjoint
  return (pbempty, pbempty)
end

function indexlength(x::Pbound)
  xempty, x1, x2 = unpack(x)
  xempty && return zero(x1.v)
  mod(x2.v - x1.v, pnnvalues)
end

function shortestcover(x::Pbound, y::Pbound)
  xempty, x1, x2 = unpack(x)
  yempty, y1, y2 = unpack(y)
  xempty && yempty && return pbempty
  xempty && return y
  yempty && return x
  (iseverything(x) || iseverything(y)) && return pbeverything
  x == y && return x

  z1 = Pbound(x1, y2)
  z2 = Pbound(y1, x2)

  # x and y cover the entire projective circle
  x1 in y && x2 in y && y1 in x && y2 in x && return pbeverything
  # One bound covers the other
  x1 in y && x2 in y && return y
  y1 in x && y2 in x && return x
  # bounds overlap but do not wrap around
  x1 in y && return z2
  x2 in y && return z1
  # bounds are disjoint, return "shortest" covering bound
  return indexlength(z1) < indexlength(z2) ? z1 : z2
end

# Make shortestcover variadic
shortestcover(x, y, zs...) = shortestcover(shortestcover(x, y), zs...)

# Useful for convex operations on Pbounds, where we know we can just
# take the range between the bottom of the first element and the top
# of the second. This method is a little dangerous because it requires
# carefully thinking about whether an operation is actually convex.
function outer(x::Pbound, y::Pbound)
  xempty, x1, x2 = unpack(x)
  yempty, y1, y2 = unpack(y)
  (xempty || yempty) && return pbempty
  Pbound(x1, y2)
end

function finiteplus(x::Pbound, y::Pbound)
  xempty, x1, x2 = unpack(x)
  yempty, y1, y2 = unpack(y)
  (xempty || yempty) && return pbempty
  outer(x1 + y1, x2 + y2)
end

function Base.(:+)(x::Pbound, y::Pbound)
  (isempty(x) || isempty(y)) && return pbempty
  (pninf in x && pninf in y) && return pbeverything

  if pninf in x
    x1, x2 = intersect(pbfinite, x)
    return shortestcover(pbinf, finiteplus(x1, y), finiteplus(x2, y))
  end

  if pninf in y
    y1, y2 = intersect(pbfinite, y)
    return shortestcover(pbinf, finiteplus(x, y1), finiteplus(x, y2))
  end

  return finiteplus(x, y)
end

function isstrictlypositive(x::Pbound)
  empty, x1, x2 = unpack(x)
  empty && return false # Is this the right thing to do?
  (pnzero in x || pninf in x) && return false
  return (x1 in pbpos && x2 in pbpos)
end

function finitenonzeropositivetimes(x::Pbound, y::Pbound)
  xempty, x1, x2 = unpack(x)
  yempty, y1, y2 = unpack(y)
  (xempty || yempty) && return pbempty

  outer(x1*y1, x2*y2)
end

function finitenonzerotimes(x::Pbound, y::Pbound)
  if !isstrictlypositive(x) && !isstrictlypositive(y)
    return finitenonzeropositivetimes(-x, -y)
  elseif !isstrictlypositive(x)
    -finitenonzeropositivetimes(-x, y)
  elseif !isstrictlypositive(y)
    -finitenonzeropositivetimes(x, -y)
  else
    finitenonzeropositivetimes(x, y)
  end
end

function finitetimes(x::Pbound, y::Pbound)
  (isempty(x) || isempty(y)) && return pbempty

  if pnzero in x && pnzero in y
    x1, x2 = intersect(pbnonzero, x)
    y1, y2 = intersect(pbnonzero, y)
    return shortestcover(
      pbzero,
      finitenonzerotimes(x1, y1),
      finitenonzerotimes(x1, y2),
      finitenonzerotimes(x2, y1),
      finitenonzerotimes(x2, y2)
    )
  end

  if pnzero in x
    x1, x2 = intersect(pbnonzero, x)
    return shortestcover(
      pbzero,
      finitenonzerotimes(x1, y),
      finitenonzerotimes(x2, y)
    )
  end

  if pnzero in y
    y1, y2 = intersect(pbnonzero, y)
    return shortestcover(
      pbzero,
      finitenonzerotimes(x, y1),
      finitenonzerotimes(x, y2)
    )
  end

  return finitenonzerotimes(x, y)
end

function Base.(:*)(x::Pbound, y::Pbound)
  (isempty(x) || isempty(y)) && return pbempty
  (pninf in x && pnzero in y) && return pbeverything
  (pnzero in x && pninf in y) && return pbeverything

  if pninf in x && pninf in y
    x1, x2 = intersect(pbfinite, x)
    y1, y2 = intersect(pbfinite, y)
    return shortestcover(
      pbinf,
      finitetimes(x1, y1),
      finitetimes(x1, y2),
      finitetimes(x2, y1),
      finitetimes(x2, y2)
    )
  end

  if pninf in x
    x1, x2 = intersect(pbfinite, x)
    return shortestcover(pbinf, finitetimes(x1, y), finitetimes(x2, y))
  end

  if pninf in y
    y1, y2 = intersect(pbfinite, y)
    return shortestcover(pbinf, finitetimes(x, y1), finitetimes(x, y2))
  end

  return finitetimes(x, y)
end

Base.(:-)(x::Pbound, y::Pbound) = x + (-y)
Base.(:/)(x::Pbound, y::Pbound) = x*recip(y)

function Base.(:(==))(x::Pbound, y::Pbound)
  isempty(x) && return isempty(y)
  iseverything(x) && return iseverything(y)
  return x.v == y.v
end

function bisect(x::Pbound)
  empty, x1, x2 = unpack(x)
  empty && return (pbempty, pbempty)
  x1 == x2 && return (Pbound(x1), pbempty)
  xc = bisect(x1, x2)
  (x1 == xc || xc == x2) && return (Pbound(x1), Pbound(x2))
  return (Pbound(x1, xc), Pbound(next(xc), x2))
end

function mergelast!(accum::Vector{Pbound}, x::Pbound)
  if length(accum) == 0
    push!(accum, x)
    return
  end
  y = last(accum)
  yempty, y1, y2 = unpack(y)
  if yempty
    # Don't really expect this. No reason to have pushed empty
    # in the first place.
    push!(accum, x)
    return
  end
  if next(y2) in x
    # Would be safer to do shortestcover
    accum[end] = y1 in x ? pbeverything : outer(y, x)
    return
  end
  push!(accum, x)
end

# TODO, kind of painful that we'll visit every possible Pnum for
# functions like x -> (-1, 1), i.e. functions that we can never bound
# away from zero.
#
# For production implementation, probably want to allow limiting how
# many bounds we'll visit, and probably want to go breadth first
# instead of depth first.
function bisectvalue!(f, x::Pnum, y::Pbound, accum::Vector{Pbound})
  fy = f(y)
  x in fy || return
  if Pbound(x) == fy
    mergelast!(accum, y)
    return
  end
  if issinglepnum(y)
    mergelast!(accum, y)
    return
  end
  y1, y2 = bisect(y)
  bisectvalue!(f, x, y1, accum)
  bisectvalue!(f, x, y2, accum)
end

function bisectvalue(f, x::Pnum, y::Pbound)
  accum = Pbound[]
  bisectvalue!(f, x, y, accum)
  return accum
end

# Special kind of everything that splits negatives and positives.
# Might want to do something more explicit here
const pbbisecteverything = Pbound(next(pninf), pninf)

bisectvalue(f, x::Pnum) = bisectvalue(f, x, pbbisecteverything)

immutable Sopn <: Number
  v::UInt8
  Sopn(b::Bitmask{UInt8}) = new(b.v)
end

rawsopn(v::UInt8) = Sopn(Bitmask(v))

const sopnempty = rawsopn(0x00)

# TODO define symmetrized versions
Base.union(x::Sopn, y::Pnum) = rawsopn(x.v | (one(y.v) << y.v))
# TODO let this fall out of promotion rules
Base.union(x::Sopn, y::Pbound) = union(x, Sopn(y))
Base.union(x::Sopn, y::Sopn) = rawsopn(x.v | y.v)
Base.intersect(x::Sopn, y::Sopn) = rawsopn(x.v & y.v)

function Base.in(x::Pnum, s::Sopn)
  m = one(x.v) << x.v
  (m & s.v) != zero(x.v)
end

const pnvrange = zero(pnnvalues):(pnnvalues - one(pnnvalues))

Base.convert(::Type{Sopn}, x::Pnum) = union(sopnempty, x)

# TODO make a way to iterate over the Pnums in a Pbound
function Base.convert(::Type{Sopn}, x::Pbound)
  out = sopnempty
  for v in pnvrange
    y = rawpnum(v)
    if y in x
      out = union(out, y)
    end
  end
  out
end

Sopn(x::Pnum) = convert(Sopn, x)
Sopn(x::Pbound) = convert(Sopn, x)

function Base.(:-)(x::Sopn)
  out = sopnempty
  for xv in pnvrange
    xp = rawpnum(xv)
    if xp in x
      out = union(out, -xp)
    end
  end
  out
end

function recip(x::Sopn)
  out = sopnempty
  for xv in pnvrange
    xp = rawpnum(xv)
    if xp in x
      out = union(out, recip(xp))
    end
  end
  out
end

function Base.(:+)(x::Sopn, y::Sopn)
  out = sopnempty
  for xv in pnvrange, yv in pnvrange
    xp = rawpnum(xv)
    yp = rawpnum(yv)
    if xp in x && yp in y
      out = union(out, xp + yp)
    end
  end
  out
end

function Base.(:*)(x::Sopn, y::Sopn)
  out = sopnempty
  for xv in pnvrange, yv in pnvrange
    xp = rawpnum(xv)
    yp = rawpnum(yv)
    if xp in x && yp in y
      out = union(out, xp*yp)
    end
  end
  out
end

Base.(:-)(x::Sopn, y::Sopn) = x + (-y)
Base.(:/)(x::Sopn, y::Sopn) = x*recip(y)

# Define some useful promotions
Base.promote_rule{T<:Real}(::Type{Pnum}, ::Type{T}) = Pnum
Base.promote_rule{T<:Real}(::Type{Pbound}, ::Type{T}) = Pbound
Base.promote_rule(::Type{Pbound}, ::Type{Pnum}) = Pbound

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

export Pnum, Pbound, @pn_str, @pb_str, recip, bisect, bisectvalue

end
