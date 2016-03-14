module Pnums

typealias NonNaNReal Union{Rational, Irrational, Integer}

# Used for indirection purposes to allow Pnum(x::Integer) to do
# conversion by value instead of bit representation, while still having
# a way to construct a Pnum from raw bits.
immutable Bitmask{T}
  v::T
end

# Pack a Pnum into the 3 trailing bits of a UInt8
# 000 -> [0, 0]
# 001 -> (0, 1)
# 010 -> [1, 1]
# 011 -> (1, /0)
# 100 -> [/0, /0]
# 101 -> (/0, -1)
# 110 -> [-1, -1]
# 111 -> (-1, 0)
immutable Pnum <: Number
  v::UInt8
  Pnum(b::Bitmask{UInt8}) = new(pnmod(Pnum, b.v))
end

storagetype(::Type{Pnum}) = UInt8
const pn3exacts = [1//1]
exacts(::Type{Pnum}) = pn3exacts
pnnvalues(::Type{Pnum}) = convert(storagetype(Pnum), 8)
pnmask(::Type{Pnum}) = pnnvalues(Pnum) - one(storagetype(Pnum))
rawpnum(::Type{Pnum}, x::storagetype(Pnum)) = Pnum(Bitmask(x))
pnmod(::Type{Pnum}, x::storagetype(Pnum)) = x & pnmask(Pnum)
Base.zero(::Type{Pnum}) = rawpnum(Pnum, zero(storagetype(Pnum)))
Base.one(::Type{Pnum}) = rawpnum(Pnum, pnnvalues(Pnum) >> 2)
pninf(::Type{Pnum}) = rawpnum(Pnum, pnnvalues(Pnum) >> 1)

index(x::Pnum) = x.v
fromindex(::Type{Pnum}, i) = rawpnum(Pnum, convert(storagetype(Pnum), i))
fromexactsindex(::Type{Pnum}, i) =
  rawpnum(Pnum, index(one(Pnum)) + (convert(storagetype(Pnum), i - 1) << 1))

Pnum(x::Real) = convert(Pnum, x)
Base.convert(::Type{Pnum}, x::Real) = _searchvalue(Pnum, x)

function Base.convert(::Type{Pnum}, x::AbstractFloat)
  isnan(x) && throw(InexactError())
  _searchvalue(Pnum, x)
end

iszero(x::Pnum) = x == zero(Pnum)
Base.isinf(x::Pnum) = x == pninf(Pnum)
isexact(x::Pnum) = trailing_ones(index(x)) == 0 # Check the ubit
# Next and prev move us anti-clockwise or clockwise around the
# projective circle
next(x::Pnum) = rawpnum(Pnum, index(x) + one(index(x)))
prev(x::Pnum) = rawpnum(Pnum, index(x) - one(index(x)))

isstrictlynegative(x::Pnum) = index(x) > index(pninf(Pnum))

function exactvalue(x::Pnum)
  isstrictlynegative(x) && return -exactvalue(-x)
  index(x) < index(one(Pnum)) && return inv(exactvalue(inv(x)))
  isinf(x) && return 1//0
  exacts(Pnum)[((index(x) - index(one(Pnum))) >> 1) + 1]
end

function _searchvalue(::Type{Pnum}, x::Real)
  x < 0 && return -convert(Pnum, -x)
  # TODO, inv(x) will fail for irrationals, and lose precision for
  # Floats. Should search by reciprocals of exact values, but I
  # couldn't figure out how to do that with searchsorted on my first
  # couple tries. Worst case, I'll just write the bisection myself.
  x == 0 && return zero(Pnum)
  isinf(x) && return pninf(Pnum)

  # Bisect exacts(Pnum) table to find value.
  lo = 0
  hi = length(exacts(Pnum)) + 1

  if x < 1
    while true
      mid = lo + ((hi - lo) >> 1)
      (mid == lo || mid == hi) && break
      lo, hi = (inv(exacts(Pnum)[mid]) > x) ? (mid, hi) : (lo, mid)
    end

    lo > 0 && inv(exacts(Pnum)[lo]) == x && return inv(fromexactsindex(Pnum, lo))
    hi <= length(exacts(Pnum)) && inv(exacts(Pnum)[hi]) == x && return inv(fromexactsindex(Pnum, hi))
    lo == 0 && return prev(one(Pnum)) # Never happens
    hi > length(exacts(Pnum)) && return next(zero(Pnum))
    return inv(next(fromexactsindex(Pnum, lo)))
  else
    while true
      mid = lo + ((hi - lo) >> 1)
      (mid == lo || mid == hi) && break
      lo, hi = (exacts(Pnum)[mid] < x) ? (mid, hi) : (lo, mid)
    end

    lo > 0 && exacts(Pnum)[lo] == x && return fromexactsindex(Pnum, lo)
    hi <= length(exacts(Pnum)) && exacts(Pnum)[hi] == x && return fromexactsindex(Pnum, hi)
    lo == 0 && return next(one(Pnum)) # Never happens
    hi > length(exacts(Pnum)) && return prev(pninf(Pnum))
    return next(fromexactsindex(Pnum, lo))
  end
end

Base.(:-)(x::Pnum) = rawpnum(Pnum, -index(x))
# Rotate 180 degrees and negate
Base.inv(x::Pnum) = rawpnum(Pnum, -(index(x) + index(pninf(Pnum))))

# Calling these slowplus and slowtimes because, in a final
# implementation, they will probably be used to generate lookup tables,
# and the lookup tables will be used for runtime arithmetic

function _exactplus(x::Pnum, y::Pnum)
  (isinf(x) || isinf(y)) ? pninf(Pnum) : convert(Pnum, exactvalue(x) + exactvalue(y))
end

# Note, returns a Pbound
function slowplus(x::Pnum, y::Pnum)
  (isinf(x) && isinf(y)) && return pbeverything(Pbound)
  (isinf(x) || isinf(y)) && return pbinf(Pbound)

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
  (isinf(x) || isinf(y)) ? pninf(Pnum) : convert(Pnum, exactvalue(x)*exactvalue(y))
end

# Note, returns a Pbound
function slowtimes(x::Pnum, y::Pnum)
  (isinf(x) && iszero(y)) && return pbeverything(Pbound)
  (iszero(x) && isinf(y)) && return pbeverything(Pbound)
  (isinf(x) || isinf(y)) && return pbinf(Pbound)
  (iszero(x) || iszero(y)) && return zero(Pbound)

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
Base.(:/)(x::Pnum, y::Pnum) = x*inv(y)

function Base.exp(x::Pnum)
  # TODO exp(pn"/0") = pb"[0, /0]" is a little painful; it should really
  # be the disconnected set {pn"0", pn"/0"}.
  isinf(x) && return Pbound(zero(Pnum), pninf(Pnum))
  xexact = isexact(x)

  x1, x2 = xexact ? (x, x) : (prev(x), next(x))
  if xexact
    y1 = y2 = Pnum(exp(exactvalue(x1)))
  else
    y1, y2 = Pnum(exp(exactvalue(x1))), Pnum(exp(exactvalue(x2)))
  end

  # The only inputs that should return an exact output are 0 and
  # infinity. Taking a conservative approach of just widening the
  # result in case floating point exp rounds to one of our exact
  # values. I don't want to deal with rounding modes for now.
  #
  # Will run into worse problems (actual incorrectness) if the
  # underlying float exp is not correctly rounded in a way that
  # causes us to find its value in the wrong Pnum.
  if isexact(y1)
    if iszero(x1)
      y1 = xexact ? y1 : next(y1)
    elseif isinf(x1)
      y1 = xexact ? y1 : next(y1)
    else
      y1 = prev(y1)
    end
  end

  if isexact(y2)
    if iszero(x2)
      y2 = xexact ? y2 : prev(y2)
    elseif isinf(x2)
      y2 = xexact ? y2 : prev(y2)
    else
      y2 = next(y2)
    end
  end

  isinf(x1) && return Pbound(next(zero(Pnum)), y2)
  isinf(x2) && return Pbound(y1, prev(pninf(Pnum)))

  Pbound(y1, y2)
end

indexlength(x::Pnum, y::Pnum) = pnmod(Pnum, index(y) - index(x))

# Index midpoint between two Pnums. Note that this is asymmetric in
# the arguments: reversing them will return a point 180 degrees away.
function bisect(x::Pnum, y::Pnum)
  rawpnum(Pnum, index(x) + (indexlength(x, y) >> 1))
end

iseverything(x::Pnum, y::Pnum) = x == next(y)

# A NonEmptyPbound is stored as a packed binary unsigned integer where
# the upper and lower halves encode Pnums. This is a low-level type.
# For general purposes, the higher level Pbound type should be used,
# since it is capable of expressing the important idea of an empty
# Pbound
immutable NonEmptyPbound <: Number
  x::Pnum
  y::Pnum
  # Always store everything in the canonical way
  function NonEmptyPbound(x::Pnum, y::Pnum)
    iseverything(x, y) ? new(next(pninf(Pnum)), pninf(Pnum)) : new(x, y)
  end
end

unpack(npb::NonEmptyPbound) = npb.x, npb.y

immutable Pbound <: Number
  isempty::Bool
  v::NonEmptyPbound
end

Base.isempty(x::Pbound) = x.isempty
unpack(x::Pbound) = (isempty(x), unpack(x.v)...)

Pbound(x::Pnum, y::Pnum) = Pbound(false, NonEmptyPbound(x, y))
Base.convert(::Type{Pbound}, x::Pnum) = Pbound(x, x)
Pbound(x::Pnum) = convert(Pbound, x)

# There are actually n^2 representations for "empty", and n
# representations for "everything", but these are the canonical ones.
Base.zero(::Type{Pbound}) = Pbound(zero(Pnum))
Base.one(::Type{Pbound}) = Pbound(one(Pnum))
pbempty(::Type{Pbound}) = Pbound(true, NonEmptyPbound(zero(Pnum), zero(Pnum)))
pbeverything(::Type{Pbound}) = Pbound(next(pninf(Pnum)), pninf(Pnum))
pbinf(::Type{Pbound}) = Pbound(pninf(Pnum))
pbfinite(::Type{Pbound}) = Pbound(next(pninf(Pnum)), prev(pninf(Pnum)))
pbnonzero(::Type{Pbound}) = Pbound(next(zero(Pnum)), prev(zero(Pnum)))
pbneg(::Type{Pbound}) = Pbound(next(pninf(Pnum)), prev(zero(Pnum)))
pbpos(::Type{Pbound}) = Pbound(next(zero(Pnum)), prev(pninf(Pnum)))

function Base.convert(::Type{Pbound}, x::Real)
  isnan(x) && return pbempty(Pbound)
  Pbound(convert(Pnum, x))
end
Pbound(x::Real) = convert(Pbound, x)
function Pbound(x::Real, y::Real)
  (isnan(x) || isnan(y)) && return pbempty(Pbound)
  Pbound(convert(Pnum, x), convert(Pnum, y))
end

function iseverything(x::Pbound)
  empty, x1, x2 = unpack(x)
  empty && return false
  x1 == next(x2)
end

function isexact(x::Pbound)
  empty, x1, x2 = unpack(x)
  empty && return false
  x1 == x2 && isexact(x1)
end

function issinglepnum(x::Pbound)
  empty, x1, x2 = unpack(x)
  empty && return false
  x1 == x2
end

function Base.(:-)(x::Pbound)
  empty, x1, x2 = unpack(x)
  empty && return x
  Pbound(-x2, -x1)
end

function Base.inv(x::Pbound)
  empty, x1, x2 = unpack(x)
  empty && return x
  Pbound(inv(x2), inv(x1))
end

function Base.complement(x::Pbound)
  empty, x1, x2 = unpack(x)
  empty && return pbeverything(Pbound)
  iseverything(x) && return pbempty(Pbound)
  Pbound(next(x2), prev(x1))
end

function Base.in(y::Pnum, x::Pbound)
  empty, x1, x2 = unpack(x)
  empty && return false
  indexlength(x1, y) <= indexlength(x1, x2)
end

function Base.intersect(x::Pbound, y::Pbound)
  xempty, x1, x2 = unpack(x)
  yempty, y1, y2 = unpack(y)
  (xempty || yempty) && return (pbempty(Pbound), pbempty(Pbound))
  iseverything(x) && return (y, pbempty(Pbound))
  iseverything(y) && return (x, pbempty(Pbound))
  x == y && return (x, pbempty(Pbound))

  # x and y cover the entire projective circle, but are not equal
  # thus they have 2 intersections
  x1 in y && x2 in y && y1 in x && y2 in x && return (Pbound(y1, x2), Pbound(x1, y2))
  # One bound covers the other
  x1 in y && x2 in y && return (x, pbempty(Pbound))
  y1 in x && y2 in x && return (y, pbempty(Pbound))
  # Bounds overlap
  x1 in y && return (Pbound(x1, y2), pbempty(Pbound))
  x2 in y && return (Pbound(y1, x2), pbempty(Pbound))
  # Bounds are disjoint
  return (pbempty(Pbound), pbempty(Pbound))
end

function shortestcover(x::Pbound, y::Pbound)
  xempty, x1, x2 = unpack(x)
  yempty, y1, y2 = unpack(y)
  xempty && yempty && return pbempty(Pbound)
  xempty && return y
  yempty && return x
  (iseverything(x) || iseverything(y)) && return pbeverything(Pbound)
  x == y && return x

  z1 = Pbound(x1, y2)
  z2 = Pbound(y1, x2)

  # x and y cover the entire projective circle
  x1 in y && x2 in y && y1 in x && y2 in x && return pbeverything(Pbound)
  # One bound covers the other
  x1 in y && x2 in y && return y
  y1 in x && y2 in x && return x
  # bounds overlap but do not wrap around
  x1 in y && return z2
  x2 in y && return z1
  # bounds are disjoint, return "shortest" covering bound
  return length(eachpnum(z1)) < length(eachpnum(z2)) ? z1 : z2
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
  (xempty || yempty) && return pbempty(Pbound)
  Pbound(x1, y2)
end

function finiteplus(x::Pbound, y::Pbound)
  xempty, x1, x2 = unpack(x)
  yempty, y1, y2 = unpack(y)
  (xempty || yempty) && return pbempty(Pbound)
  outer(x1 + y1, x2 + y2)
end

function Base.(:+)(x::Pbound, y::Pbound)
  (isempty(x) || isempty(y)) && return pbempty(Pbound)
  (pninf(Pnum) in x && pninf(Pnum) in y) && return pbeverything(Pbound)

  if pninf(Pnum) in x
    x1, x2 = intersect(pbfinite(Pbound), x)
    return shortestcover(pbinf(Pbound), finiteplus(x1, y), finiteplus(x2, y))
  end

  if pninf(Pnum) in y
    y1, y2 = intersect(pbfinite(Pbound), y)
    return shortestcover(pbinf(Pbound), finiteplus(x, y1), finiteplus(x, y2))
  end

  return finiteplus(x, y)
end

function isstrictlypositive(x::Pbound)
  empty, x1, x2 = unpack(x)
  empty && return false
  (zero(Pnum) in x || pninf(Pnum) in x) && return false
  return (x1 in pbpos(Pbound) && x2 in pbpos(Pbound))
end

function finitenonzeropositivetimes(x::Pbound, y::Pbound)
  xempty, x1, x2 = unpack(x)
  yempty, y1, y2 = unpack(y)
  (xempty || yempty) && return pbempty(Pbound)

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
  (isempty(x) || isempty(y)) && return pbempty(Pbound)

  if zero(Pnum) in x && zero(Pnum) in y
    x1, x2 = intersect(pbnonzero(Pbound), x)
    y1, y2 = intersect(pbnonzero(Pbound), y)
    return shortestcover(
      zero(Pbound),
      finitenonzerotimes(x1, y1),
      finitenonzerotimes(x1, y2),
      finitenonzerotimes(x2, y1),
      finitenonzerotimes(x2, y2)
    )
  end

  if zero(Pnum) in x
    x1, x2 = intersect(pbnonzero(Pbound), x)
    return shortestcover(
      zero(Pbound),
      finitenonzerotimes(x1, y),
      finitenonzerotimes(x2, y)
    )
  end

  if zero(Pnum) in y
    y1, y2 = intersect(pbnonzero(Pbound), y)
    return shortestcover(
      zero(Pbound),
      finitenonzerotimes(x, y1),
      finitenonzerotimes(x, y2)
    )
  end

  return finitenonzerotimes(x, y)
end

function Base.(:*)(x::Pbound, y::Pbound)
  (isempty(x) || isempty(y)) && return pbempty(Pbound)
  (pninf(Pnum) in x && zero(Pnum) in y) && return pbeverything(Pbound)
  (zero(Pnum) in x && pninf(Pnum) in y) && return pbeverything(Pbound)

  if pninf(Pnum) in x && pninf(Pnum) in y
    x1, x2 = intersect(pbfinite(Pbound), x)
    y1, y2 = intersect(pbfinite(Pbound), y)
    return shortestcover(
      pbinf(Pbound),
      finitetimes(x1, y1),
      finitetimes(x1, y2),
      finitetimes(x2, y1),
      finitetimes(x2, y2)
    )
  end

  if pninf(Pnum) in x
    x1, x2 = intersect(pbfinite(Pbound), x)
    return shortestcover(pbinf(Pbound), finitetimes(x1, y), finitetimes(x2, y))
  end

  if pninf(Pnum) in y
    y1, y2 = intersect(pbfinite(Pbound), y)
    return shortestcover(pbinf(Pbound), finitetimes(x, y1), finitetimes(x, y2))
  end

  return finitetimes(x, y)
end

Base.(:-)(x::Pbound, y::Pbound) = x + (-y)
Base.(:/)(x::Pbound, y::Pbound) = x*inv(y)

function Base.(:(==))(x::Pbound, y::Pbound)
  xeverything, yeverything = iseverything(x), iseverything(y)
  (xeverything && yeverything) && return true
  (xeverything || yeverything) && return false
  xempty, x1, x2 = unpack(x)
  yempty, y1, y2 = unpack(y)
  (xempty && yempty) && return true
  (xempty || yempty) && return false
  return x1 == y1 && x2 == y2
end

function Base.exp(x::Pbound)
  xempty, x1, x2 = unpack(x)
  xempty && return x
  pninf(Pnum) in x && return Pbound(zero(Pnum), pninf(Pnum))
  outer(exp(x1), exp(x2))
end

function bisect(x::Pbound)
  empty, x1, x2 = unpack(x)
  empty && return (pbempty(Pbound), pbempty(Pbound))
  x1 == x2 && return (Pbound(x1), pbempty(Pbound))
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
    accum[end] = y1 in x ? pbeverything(Pbound) : outer(y, x)
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

bisectvalue(f, x::Pnum) = bisectvalue(f, x, pbeverything(Pbound))

immutable PboundIterator
  pb::Pbound
  len::Int
end

function eachpnum(x::Pbound)
  xempty, x1, x2 = unpack(x)
  xempty && return PboundIterator(x, zero(index(x1)))
  PboundIterator(x, indexlength(x1, x2) + one(index(x1)))
end

function Base.start(x::PboundIterator)
  xempty, x1, x2 = unpack(x.pb)
  (x1, 1)
end

Base.next(x::PboundIterator, t) = (first(t), (next(first(t)), last(t) + 1))

function Base.done(x::PboundIterator, t)
  last(t) > x.len
end

Base.eltype(x::PboundIterator) = Pnum
Base.length(x::PboundIterator) = x.len

immutable Sopn <: Number
  s::IntSet
  Sopn() = new(IntSet())
end

Sopn(itr) = reduce(union!, Sopn(), itr)

# TODO define symmetrized versions
function Base.union!(x::Sopn, y::Pnum)
  push!(x.s, index(y) + 1)
  x
end
# TODO let this fall out of promotion rules
Base.union!(x::Sopn, y::Pbound) = reduce(union!, x, eachpnum(y))
Base.union!(x::Sopn, y::Sopn) = reduce(union!, x, eachpnum(y))

Base.convert(::Type{Sopn}, x::Pnum) = union!(Sopn(), x)
Base.convert(::Type{Sopn}, x::Pbound) = Sopn(eachpnum(x))
Sopn(x::Pnum) = convert(Sopn, x)
Sopn(x::Pbound) = convert(Sopn, x)

Base.in(x::Pnum, s::Sopn) = (index(x) + 1) in s
Base.isempty(x::Sopn) = isempty(x.s)
Base.(:(==))(x::Sopn, y::Sopn) = x.s == y.s

type SopnIterator
  s::Sopn
end

eachpnum(x::Sopn) = SopnIterator(x)

function Base.start(x::SopnIterator)
  start(x.s.s)
end

function Base.next(x::SopnIterator, state)
  id, state = Base.next(x.s.s, state)
  (fromindex(Pnum, id - 1), state)
end

function Base.done(x::SopnIterator, state)
  done(x.s.s, state)
end

Base.copy(x::Sopn) = Sopn(eachpnum(x))

Base.eltype(x::SopnIterator) = Pnum

Base.(:-)(x::Sopn) = mapreduce((-), union!, Sopn(), eachpnum(x))
Base.inv(x::Sopn) = mapreduce(inv, union!, Sopn(), eachpnum(x))

# TODO simplify 2 arg functions with metaprogramming
function Base.(:+)(x::Sopn, y::Sopn)
  out = Sopn()
  for xp in eachpnum(x), yp in eachpnum(y)
    union!(out, xp + yp)
  end
  out
end

function Base.(:-)(x::Sopn, y::Sopn)
  out = Sopn()
  for xp in eachpnum(x), yp in eachpnum(y)
    union!(out, xp - yp)
  end
  out
end

function Base.(:*)(x::Sopn, y::Sopn)
  out = Sopn()
  for xp in eachpnum(x), yp in eachpnum(y)
    union!(out, xp*yp)
  end
  out
end

function Base.(:/)(x::Sopn, y::Sopn)
  out = Sopn()
  for xp in eachpnum(x), yp in eachpnum(y)
    union!(out, xp/yp)
  end
  out
end

Base.exp(x::Sopn) = mapreduce(exp, union!, Sopn(), eachpnum(x))

# Define some useful promotions
Base.promote_rule{T<:Real}(::Type{Pnum}, ::Type{T}) = Pnum
Base.promote_rule{T<:Real}(::Type{Pbound}, ::Type{T}) = Pbound
Base.promote_rule(::Type{Pbound}, ::Type{Pnum}) = Pbound

include("./io.jl")

export Pnum, Pbound, @pn_str, @pb_str, bisect, bisectvalue, eachpnum

end
