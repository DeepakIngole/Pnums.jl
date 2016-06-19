Base.zero{T<:AbstractPnum}(::Type{T}) = rawpnum(T, zero(storagetype(T)))
Base.one{T<:AbstractPnum}(::Type{T}) = rawpnum(
  T,
  convert(storagetype(T), pnnvalues(T) >> 2)
)
pninf{T<:AbstractPnum}(::Type{T}) = rawpnum(
  T,
  convert(storagetype(T), pnnvalues(T) >> 1)
)

index(x::AbstractPnum) = x.v
fromindex{T<:AbstractPnum}(::Type{T}, i) = rawpnum(T, convert(storagetype(T), i))
fromexactsindex{T<:AbstractPnum}(::Type{T}, i) =
  rawpnum(T, index(one(T)) + (convert(storagetype(T), i - 1) << 1))

pninf(x::AbstractPnum) = pninf(typeof(x))
exacts(x::AbstractPnum) = exacts(typeof(x))

iszero(x::AbstractPnum) = x == zero(x)
Base.isinf(x::AbstractPnum) = x == pninf(x)
isexact(x::AbstractPnum) = trailing_ones(index(x)) == 0 # Check the ubit
# Next and prev move us anti-clockwise or clockwise around the
# projective circle
nextpnum(x::AbstractPnum) = rawpnum(typeof(x), index(x) + one(index(x)))
prevpnum(x::AbstractPnum) = rawpnum(typeof(x), index(x) - one(index(x)))

isstrictlynegative(x::AbstractPnum) = index(x) > index(pninf(x))

function exactvalue(x::AbstractPnum)
  isstrictlynegative(x) && return -exactvalue(-x)
  index(x) < index(one(x)) && return inv(exactvalue(inv(x)))
  isinf(x) && return 1//0
  exacts(x)[((index(x) - index(one(x))) >> 1) + 1]
end

function _searchvalue{T<:AbstractPnum}(::Type{T}, x::Real)
  x < 0 && return -convert(T, -x)
  # TODO, inv(x) will fail for irrationals, and lose precision for
  # Floats. Should search by reciprocals of exact values, but I
  # couldn't figure out how to do that with searchsorted on my first
  # couple tries. Worst case, I'll just write the bisection myself.
  x == 0 && return zero(T)
  isinf(x) && return pninf(T)

  # Bisect exacts table to find value.
  etable = exacts(T)
  lo = 0
  hi = length(etable) + 1

  if x < 1
    while true
      mid = lo + ((hi - lo) >> 1)
      (mid == lo || mid == hi) && break
      lo, hi = (inv(etable[mid]) > x) ? (mid, hi) : (lo, mid)
    end

    lo > 0 && inv(etable[lo]) == x && return inv(fromexactsindex(T, lo))
    hi <= length(etable) && inv(etable[hi]) == x && return inv(fromexactsindex(T, hi))
    lo == 0 && return prevpnum(one(T)) # Never happens
    hi > length(etable) && return nextpnum(zero(T))
    return inv(nextpnum(fromexactsindex(T, lo)))
  else
    while true
      mid = lo + ((hi - lo) >> 1)
      (mid == lo || mid == hi) && break
      lo, hi = (etable[mid] < x) ? (mid, hi) : (lo, mid)
    end

    lo > 0 && etable[lo] == x && return fromexactsindex(T, lo)
    hi <= length(etable) && etable[hi] == x && return fromexactsindex(T, hi)
    lo == 0 && return nextpnum(one(T)) # Never happens
    hi > length(etable) && return prevpnum(pninf(T))
    return nextpnum(fromexactsindex(T, lo))
  end
end

function Base.convert{T<:AbstractPnum}(::Type{T}, x::AbstractFloat)
  isnan(x) && throw(InexactError())
  _searchvalue(T, x)
end

Base.convert{T<:AbstractPnum}(::Type{T}, x::Real) = _searchvalue(T, x)

Base.(:-)(x::AbstractPnum) = rawpnum(typeof(x), -index(x))
# Rotate 180 degrees and negate
Base.inv(x::AbstractPnum) = rawpnum(typeof(x), -(index(x) + index(pninf(x))))

# Calling these slowplus and slowtimes because, in a final
# implementation, they will probably be used to generate lookup tables,
# and the lookup tables will be used for runtime arithmetic

function _exactplus{T<:AbstractPnum}(x::T, y::T)
  rx = widen(exactvalue(x))
  ry = widen(exactvalue(y))
  (isinf(x) || isinf(y)) ? pninf(T) : convert(T, rx + ry)
end

# Note, returns a Pbound
function slowplus{T<:AbstractPnum}(x::T, y::T)
  (isinf(x) && isinf(y)) && return pbeverything(Pbound{T})
  (isinf(x) || isinf(y)) && return pbinf(Pbound{T})

  xexact, yexact = isexact(x), isexact(y)
  bothexact = xexact && yexact

  x1, x2 = xexact ? (x, x) : (prevpnum(x), nextpnum(x))
  y1, y2 = yexact ? (y, y) : (prevpnum(y), nextpnum(y))

  z1 = _exactplus(x1, y1)
  z2 = _exactplus(x2, y2)

  z1 = (!bothexact && isexact(z1)) ? nextpnum(z1) : z1
  z2 = (!bothexact && isexact(z2)) ? prevpnum(z2) : z2

  Pbound(z1, z2)
end

function _exacttimes{T<:AbstractPnum}(x::T, y::T)
  rx = widen(exactvalue(x))
  ry = widen(exactvalue(y))
  (isinf(x) || isinf(y)) ? pninf(T) : convert(T, rx*ry)
end

# Note, returns a Pbound
function slowtimes{T<:AbstractPnum}(x::T, y::T)
  (isinf(x) && iszero(y)) && return pbeverything(Pbound{T})
  (iszero(x) && isinf(y)) && return pbeverything(Pbound{T})
  (isinf(x) || isinf(y)) && return pbinf(Pbound{T})
  (iszero(x) || iszero(y)) && return zero(Pbound{T})

  xexact, yexact = isexact(x), isexact(y)
  bothexact = xexact && yexact

  x1, x2 = xexact ? (x, x) : (prevpnum(x), nextpnum(x))
  y1, y2 = yexact ? (y, y) : (prevpnum(y), nextpnum(y))

  if (isstrictlynegative(y))
    x1, x2 = x2, x1
  end

  if (isstrictlynegative(x))
    y1, y2 = y2, y1
  end

  z1 = _exacttimes(x1, y1)
  z2 = _exacttimes(x2, y2)

  z1 = (!bothexact && isexact(z1)) ? nextpnum(z1) : z1
  z2 = (!bothexact && isexact(z2)) ? prevpnum(z2) : z2

  Pbound(z1, z2)
end

# TODO plan to replace these with lut operations at some point (maybe)
Base.(:+){T<:AbstractPnum}(x::T, y::T) = slowplus(x, y)
Base.(:-){T<:AbstractPnum}(x::T, y::T) = x + (-y)
Base.(:*){T<:AbstractPnum}(x::T, y::T) = slowtimes(x, y)
Base.(:/){T<:AbstractPnum}(x::T, y::T) = x*inv(y)
Base.(:(==))(x::AbstractPnum, y::Real) = isexact(x) && exactvalue(x) == y
Base.(:(==))(x::Real, y::AbstractPnum) = y == x

function Base.(:^)(x::AbstractPnum, n::Integer)
  T = typeof(x)
  xexact = isexact(x)

  # TODO converting to floats to avoid underflow/overflow problems.
  # Should figure out if there's a decent way to work exactly
  x1, x2 = xexact ? (x, x) : (prevpnum(x), nextpnum(x))
  if xexact
    y1 = y2 = T(convert(Float64, exactvalue(x1))^n)
  else
    y1, y2 = T(convert(Float64, exactvalue(x1))^n), T(convert(Float64, exactvalue(x2))^n)
  end

  if iseven(n) && isstrictlynegative(x)
    y1, y2 = y2, y1
  end

  y1 = !xexact && isexact(y1) ? nextpnum(y1) : y1
  y2 = !xexact && isexact(y2) ? prevpnum(y2) : y2

  Pbound(y1, y2)
end

function Base.exp{T<:AbstractPnum}(x::T)
  # TODO exp(pn"/0") = pb"[0, /0]" is a little painful; it should really
  # be the disconnected set {pn"0", pn"/0"}.
  isinf(x) && return Pbound(zero(T), pninf(T))
  xexact = isexact(x)

  x1, x2 = xexact ? (x, x) : (prevpnum(x), nextpnum(x))
  if xexact
    y1 = y2 = T(exp(exactvalue(x1)))
  else
    y1, y2 = T(exp(exactvalue(x1))), T(exp(exactvalue(x2)))
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
      y1 = xexact ? y1 : nextpnum(y1)
    elseif isinf(x1)
      y1 = xexact ? y1 : nextpnum(y1)
    else
      y1 = prevpnum(y1)
    end
  end

  if isexact(y2)
    if iszero(x2)
      y2 = xexact ? y2 : prevpnum(y2)
    elseif isinf(x2)
      y2 = xexact ? y2 : prevpnum(y2)
    else
      y2 = nextpnum(y2)
    end
  end

  isinf(x1) && return Pbound(nextpnum(zero(T)), y2)
  isinf(x2) && return Pbound(y1, prevpnum(pninf(T)))

  Pbound(y1, y2)
end

function Base.sqrt{T<:AbstractPnum}(x::T)
  isstrictlynegative(x) && return pbempty(Pbound{T})
  xexact = isexact(x)

  x1, x2 = xexact ? (x, x) : (prevpnum(x), nextpnum(x))
  if xexact
    y1 = y2 = T(sqrt(exactvalue(x1)))
  else
    y1, y2 = T(sqrt(exactvalue(x1))), T(sqrt(exactvalue(x2)))
  end

  if isexact(y1)
    if !xexact
      y1 = nextpnum(y1)
    elseif y1*y1 != x1
      y1 = prevpnum(y1)
    end
  end

  if isexact(y2)
    if !xexact
      y2 = prevpnum(y2)
    elseif y2*y2 != x1
      y2 = nextpnum(y2)
    end
  end

  Pbound(y1, y2)
end

indexlength{T<:AbstractPnum}(x::T, y::T) = pnmod(T, index(y) - index(x))

# Index midpoint between two Pnums. Note that this is asymmetric in
# the arguments: reversing them will return a point 180 degrees away.
function bisect{T<:AbstractPnum}(x::T, y::T)
  rawpnum(T, index(x) + (indexlength(x, y) >> 1))
end

iseverything{T<:AbstractPnum}(x::T, y::T) = x == nextpnum(y)

Base.eltype{T}(::Type{NonEmptyPbound{T}}) = T
Pbound{T<:AbstractPnum}(x::T, y::T) = Pbound(false, NonEmptyPbound{T}(x, y))
Base.convert{T<:AbstractPnum}(::Type{Pbound{T}}, x::T) = Pbound(x, x)
Pbound(x::AbstractPnum) = convert(Pbound{typeof(x)}, x)
Base.eltype{T<:AbstractPnum}(::Type{Pbound{T}}) = T

# There are actually n^2 representations for "empty", and n
# representations for "everything", but these are the canonical ones.
Base.zero{T<:AbstractPnum}(::Type{Pbound{T}}) = Pbound(zero(T))
Base.one{T<:AbstractPnum}(::Type{Pbound{T}}) = Pbound(one(T))
pbempty{T<:AbstractPnum}(::Type{Pbound{T}}) = Pbound(true, NonEmptyPbound(zero(T), zero(T)))
pbeverything{T<:AbstractPnum}(::Type{Pbound{T}}) = Pbound(nextpnum(pninf(T)), pninf(T))
pbinf{T<:AbstractPnum}(::Type{Pbound{T}}) = Pbound(pninf(T))
pbfinite{T<:AbstractPnum}(::Type{Pbound{T}}) = Pbound(nextpnum(pninf(T)), prevpnum(pninf(T)))
pbnonzero{T<:AbstractPnum}(::Type{Pbound{T}}) = Pbound(nextpnum(zero(T)), prevpnum(zero(T)))
pbneg{T<:AbstractPnum}(::Type{Pbound{T}}) = Pbound(nextpnum(pninf(T)), prevpnum(zero(T)))
pbpos{T<:AbstractPnum}(::Type{Pbound{T}}) = Pbound(nextpnum(zero(T)), prevpnum(pninf(T)))

pbempty(x::Pbound) = pbempty(typeof(x))
pbeverything(x::Pbound) = pbeverything(typeof(x))
pbinf(x::Pbound) = pbinf(typeof(x))
pbfinite(x::Pbound) = pbfinite(typeof(x))
pbnonzero(x::Pbound) = pbnonzero(typeof(x))
pbneg(x::Pbound) = pbneg(typeof(x))
pbpos(x::Pbound) = pbpos(typeof(x))

function Base.convert{T<:Pbound}(::Type{T}, x::Real)
  isnan(x) && return pbempty(T)
  Pbound(convert(eltype(T), x))
end
function Base.convert{T<:Pbound}(::Type{T}, x::Real, y::Real)
  (isnan(x) || isnan(y)) && return pbempty(T)
  Pbound(convert(eltype(T), x), convert(eltype(T), y))
end
Base.call{T<:Pbound}(::Type{T}, x::Real, y::Real) = convert(T, x, y)

function iseverything(x::Pbound)
  empty, x1, x2 = unpack(x)
  empty && return false
  iseverything(x1, x2)
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
  empty && return pbeverything(x)
  iseverything(x) && return pbempty(x)
  Pbound(nextpnum(x2), prevpnum(x1))
end

function Base.in{T<:AbstractPnum}(y::T, x::Pbound{T})
  empty, x1, x2 = unpack(x)
  empty && return false
  indexlength(x1, y) <= indexlength(x1, x2)
end

Base.in{T<:AbstractPnum}(x::Real, y::Pbound{T}) = convert(T, x) in y

function Base.intersect{T<:Pbound}(x::T, y::T)
  xempty, x1, x2 = unpack(x)
  yempty, y1, y2 = unpack(y)
  (xempty || yempty) && return (pbempty(T), pbempty(T))
  iseverything(x) && return (y, pbempty(T))
  iseverything(y) && return (x, pbempty(T))
  x == y && return (x, pbempty(T))

  # x and y cover the entire projective circle, but are not equal
  # thus they have 2 intersections
  x1 in y && x2 in y && y1 in x && y2 in x && return (Pbound(y1, x2), Pbound(x1, y2))
  # One bound covers the other
  x1 in y && x2 in y && return (x, pbempty(T))
  y1 in x && y2 in x && return (y, pbempty(T))
  # Bounds overlap
  x1 in y && return (Pbound(x1, y2), pbempty(T))
  x2 in y && return (Pbound(y1, x2), pbempty(T))
  # Bounds are disjoint
  return (pbempty(T), pbempty(T))
end

function shortestcover{T<:Pbound}(x::T, y::T)
  xempty, x1, x2 = unpack(x)
  yempty, y1, y2 = unpack(y)
  xempty && yempty && return pbempty(T)
  xempty && return y
  yempty && return x
  (iseverything(x) || iseverything(y)) && return pbeverything(T)
  x == y && return x

  z1 = Pbound(x1, y2)
  z2 = Pbound(y1, x2)

  # x and y cover the entire projective circle
  x1 in y && x2 in y && y1 in x && y2 in x && return pbeverything(T)
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
function outer{T<:Pbound}(x::T, y::T)
  xempty, x1, x2 = unpack(x)
  yempty, y1, y2 = unpack(y)
  (xempty || yempty) && return pbempty(T)
  Pbound(x1, y2)
end

function finiteplus{T<:Pbound}(x::T, y::T)
  xempty, x1, x2 = unpack(x)
  yempty, y1, y2 = unpack(y)
  (xempty || yempty) && return pbempty(T)
  outer(x1 + y1, x2 + y2)
end

function Base.(:+){T<:Pbound}(x::T, y::T)
  (isempty(x) || isempty(y)) && return pbempty(T)
  (pninf(eltype(T)) in x && pninf(eltype(T)) in y) && return pbeverything(T)

  if pninf(eltype(T)) in x
    x1, x2 = intersect(pbfinite(T), x)
    return shortestcover(pbinf(T), finiteplus(x1, y), finiteplus(x2, y))
  end

  if pninf(eltype(T)) in y
    y1, y2 = intersect(pbfinite(T), y)
    return shortestcover(pbinf(T), finiteplus(x, y1), finiteplus(x, y2))
  end

  return finiteplus(x, y)
end

function isstrictlypositive(x::Pbound)
  empty, x1, x2 = unpack(x)
  empty && return false
  (zero(eltype(x)) in x || pninf(eltype(x)) in x) && return false
  return (x1 in pbpos(x) && x2 in pbpos(x))
end

function finitenonzeropositivetimes{T<:Pbound}(x::T, y::T)
  xempty, x1, x2 = unpack(x)
  yempty, y1, y2 = unpack(y)
  (xempty || yempty) && return pbempty(T)

  outer(x1*y1, x2*y2)
end

function finitenonzerotimes{T<:Pbound}(x::T, y::T)
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

function finitetimes{T<:Pbound}(x::T, y::T)
  (isempty(x) || isempty(y)) && return pbempty(T)

  if zero(eltype(T)) in x && zero(eltype(T)) in y
    x1, x2 = intersect(pbnonzero(T), x)
    y1, y2 = intersect(pbnonzero(T), y)
    return shortestcover(
      zero(T),
      finitenonzerotimes(x1, y1),
      finitenonzerotimes(x1, y2),
      finitenonzerotimes(x2, y1),
      finitenonzerotimes(x2, y2)
    )
  end

  if zero(eltype(T)) in x
    x1, x2 = intersect(pbnonzero(T), x)
    return shortestcover(
      zero(T),
      finitenonzerotimes(x1, y),
      finitenonzerotimes(x2, y)
    )
  end

  if zero(eltype(T)) in y
    y1, y2 = intersect(pbnonzero(T), y)
    return shortestcover(
      zero(T),
      finitenonzerotimes(x, y1),
      finitenonzerotimes(x, y2)
    )
  end

  return finitenonzerotimes(x, y)
end

function Base.(:*){T<:Pbound}(x::T, y::T)
  (isempty(x) || isempty(y)) && return pbempty(T)
  (pninf(eltype(T)) in x && zero(eltype(T)) in y) && return pbeverything(T)
  (zero(eltype(T)) in x && pninf(eltype(T)) in y) && return pbeverything(T)

  if pninf(eltype(T)) in x && pninf(eltype(T)) in y
    x1, x2 = intersect(pbfinite(T), x)
    y1, y2 = intersect(pbfinite(T), y)
    return shortestcover(
      pbinf(T),
      finitetimes(x1, y1),
      finitetimes(x1, y2),
      finitetimes(x2, y1),
      finitetimes(x2, y2)
    )
  end

  if pninf(eltype(T)) in x
    x1, x2 = intersect(pbfinite(T), x)
    return shortestcover(pbinf(T), finitetimes(x1, y), finitetimes(x2, y))
  end

  if pninf(eltype(T)) in y
    y1, y2 = intersect(pbfinite(T), y)
    return shortestcover(pbinf(T), finitetimes(x, y1), finitetimes(x, y2))
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

function Base.(:(==))(x::Pbound, y::Real)
  empty, x1, x2 = unpack(x)
  empty && return false
  x1 == x2 == y
end
Base.(:(==))(x::Real, y::Pbound) = y == x


function finitenonzeropow(x::Pbound, n::Integer)
  xempty, x1, x2 = unpack(x)
  xempty && return pbempty(x)

  if isstrictlypositive(x) || isodd(n)
    return outer(x1^n, x2^n)
  else
    return outer(x2^n, x1^n)
  end
end

function finitepow{T<:AbstractPnum}(x::Pbound{T}, n::Integer)
  isempty(x) && return pbempty(x)

  if !(zero(T) in x)
    return finitenonzeropow(x, n)
  end

  x1, x2 = intersect(pbnonzero(x), x)

  return shortestcover(
    finitenonzeropow(x1, n),
    zero(x),
    finitenonzeropow(x2, n)
  )
end

function Base.(:^){T<:AbstractPnum}(x::Pbound{T}, n::Integer)
  xempty, x1, x2 = unpack(x)
  xempty && return pbempty(x)

  if !(pninf(T) in x)
    return finitepow(x, n)
  end

  x1, x2 = intersect(pbfinite(x), x)

  return shortestcover(
    finitepow(x1, n),
    pbinf(x),
    finitepow(x2, n)
  )
end

function Base.exp(x::Pbound)
  xempty, x1, x2 = unpack(x)
  xempty && return pbempty(x)
  pninf(eltype(x)) in x && return Pbound(zero(eltype(x)), pninf(eltype(x)))
  outer(exp(x1), exp(x2))
end

function possqrt(x::Pbound)
  xempty, x1, x2 = unpack(x)
  xempty && return pbempty(x)
  outer(sqrt(x1), sqrt(x2))
end

function Base.sqrt(x::Pbound)
  T = typeof(x)
  isempty(x) && return pbempty(x)

  x1, x2 = intersect(complement(pbneg(T)), x)
  shortestcover(possqrt(x1), possqrt(x2))
end

function bisect(x::Pbound)
  empty, x1, x2 = unpack(x)
  empty && return (pbempty(x), pbempty(x))
  x1 == x2 && return (Pbound(x1), pbempty(x))
  xc = bisect(x1, x2)
  (x1 == xc || xc == x2) && return (Pbound(x1), Pbound(x2))
  return (Pbound(x1, xc), Pbound(nextpnum(xc), x2))
end

function mergelast!{T<:Pbound}(accum::Vector{T}, x::T)
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
  if nextpnum(y2) in x
    # Would be safer to do shortestcover
    accum[end] = y1 in x ? pbeverything(T) : outer(y, x)
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
function bisectvalue!{T<:AbstractPnum}(f, x::Pbound{T}, y::T, accum::Vector{Pbound{T}})
  fx = f(x)
  y in fx || return
  if y == fx
    mergelast!(accum, x)
    return
  end
  if issinglepnum(x)
    mergelast!(accum, x)
    return
  end
  x1, x2 = bisect(x)
  bisectvalue!(f, x1, y, accum)
  bisectvalue!(f, x2, y, accum)
end

function bisectvalue{T<:AbstractPnum}(f, x::Pbound{T}, y::T)
  accum = Pbound{T}[]
  bisectvalue!(f, x, y, accum)
  return accum
end

findroots!{T<:AbstractPnum}(f, x::Pbound{T}, accum::Vector{Pbound{T}}) =
  bisectvalue!(f, x, zero(T), accum)

findroots{T<:AbstractPnum}(f, x::Pbound{T}) = bisectvalue(f, x, zero(T))

function _max{T<:AbstractPnum}(x::T, y::T)
  infindex = index(pninf(T))
  maxindex = max(pnmod(T,index(x) + infindex), pnmod(T,index(y) + infindex))
  fromindex(T, pnmod(T, maxindex - infindex))
end

# TODO making this "private" because the value it returns is just a
# lower bound on the maximum value. It would be a better general
# purpose API to return a Pbound from this function, but that isn't
# necessary for using it as a subroutine of findmaximum.
function _findmaximumvalue!{T<:AbstractPnum}(f, x::Pbound{T}, trials::Vector{Pbound{T}})
  maxmin = pninf(T)
  push!(trials, x)
  while length(trials) > 0
    x = pop!(trials)
    y = f(x)
    yempty, y1, y2 = unpack(y)
    yempty && continue
    if pninf(T) in y
      if !issinglepnum(x)
        push!(trials, bisect(x)...)
      end
    else
      maxmin = _max(y1, maxmin)
      if !issinglepnum(x)
        if y2 in Pbound(maxmin, pninf(T))
          push!(trials, bisect(x)...)
        end
      end
    end
  end

  return maxmin
end

function findmaximum{T<:AbstractPnum}(f, x::Pbound{T})
  maxmin = _findmaximumvalue!(f, x, Pbound{T}[])
  bisectvalue(f, x, maxmin)
end

immutable PboundIterator{T}
  pb::Pbound{T}
  len::Int
end

function eachpnum(x::Pbound)
  xempty, x1, x2 = unpack(x)
  xempty && return PboundIterator(x, 0)
  PboundIterator(x, indexlength(x1, x2) + 1)
end

eachpnum{T}(x::Type{Pbound{T}}) = eachpnum(pbeverything(x))
eachpnum{T<:AbstractPnum}(x::Type{T}) = eachpnum(pbeverything(Pbound{T}))

function Base.start(x::PboundIterator)
  xempty, x1, x2 = unpack(x.pb)
  (x1, 1)
end

Base.next(x::PboundIterator, t) = (first(t), (nextpnum(first(t)), last(t) + 1))

function Base.done(x::PboundIterator, t)
  last(t) > x.len
end

Base.eltype{T<:AbstractPnum}(x::PboundIterator{T}) = T
Base.length(x::PboundIterator) = x.len

# SOPN

Base.eltype{T}(::Type{Sopn{T}}) = T

function Base.union!{T<:AbstractPnum}(x::Sopn{T}, y::T)
  push!(x.s, index(y) + 1)
  x
end
Base.union!{T<:AbstractPnum}(x::Sopn{T}, y::Pbound{T}) = reduce(union!, x, eachpnum(y))
Base.union!{T<:Sopn}(x::T, y::T) = reduce(union!, x, eachpnum(y))

Base.convert{T<:AbstractPnum}(::Type{Sopn{T}}, x::T) = union!(Sopn(T), x)
Base.convert{T<:AbstractPnum}(::Type{Sopn{T}}, x::Pbound{T}) = Sopn(eachpnum(x))
Sopn{T<:AbstractPnum}(x::T) = convert(Sopn{T}, x)
Sopn{T<:AbstractPnum}(x::Pbound{T}) = convert(Sopn{T}, x)

Base.in{T<:AbstractPnum}(x::T, s::Sopn{T}) = (index(x) + 1) in s
Base.isempty(x::Sopn) = isempty(x.s)
Base.(:(==)){T<:Sopn}(x::T, y::T) = x.s == y.s

type SopnIterator{T}
  s::Sopn{T}
end

eachpnum(x::Sopn) = SopnIterator(x)

function Base.start(x::SopnIterator)
  start(x.s.s)
end

function Base.next(x::SopnIterator, state)
  id, state = next(x.s.s, state)
  (fromindex(eltype(x), id - 1), state)
end

function Base.done(x::SopnIterator, state)
  done(x.s.s, state)
end

Base.copy(x::Sopn) = Sopn(eachpnum(x))

Base.eltype{T}(x::SopnIterator{T}) = T

Base.(:-){T}(x::Sopn{T}) = mapreduce((-), union!, Sopn(T), eachpnum(x))
Base.inv{T}(x::Sopn{T}) = mapreduce(inv, union!, Sopn(T), eachpnum(x))

# TODO simplify 2 arg functions with metaprogramming
function Base.(:+){T}(x::Sopn{T}, y::Sopn{T})
  out = Sopn(T)
  for xp in eachpnum(x), yp in eachpnum(y)
    union!(out, xp + yp)
  end
  out
end

function Base.(:-){T}(x::Sopn{T}, y::Sopn{T})
  out = Sopn(T)
  for xp in eachpnum(x), yp in eachpnum(y)
    union!(out, xp - yp)
  end
  out
end

function Base.(:*){T}(x::Sopn{T}, y::Sopn{T})
  out = Sopn(T)
  for xp in eachpnum(x), yp in eachpnum(y)
    union!(out, xp*yp)
  end
  out
end

function Base.(:/){T}(x::Sopn{T}, y::Sopn{T})
  out = Sopn(T)
  for xp in eachpnum(x), yp in eachpnum(y)
    union!(out, xp/yp)
  end
  out
end

function Base.(:^){T}(x::Sopn{T}, n::Integer)
  out = Sopn(T)
  for xp in eachpnum(x)
    union!(out, xp^n)
  end
  out
end

Base.exp{T}(x::Sopn{T}) = mapreduce(exp, union!, Sopn(T), eachpnum(x))
Base.sqrt{T}(x::Sopn{T}) = mapreduce(sqrt, union!, Sopn(T), eachpnum(x))
