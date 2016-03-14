# Used for indirection purposes to allow Pnum(x::Integer) to do
# conversion by value instead of bit representation, while still having
# a way to construct a Pnum from raw bits.
immutable Bitmask{T<:Integer}
  v::T
end

abstract AbstractPnum <: Number

immutable Pnum <: AbstractPnum
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

Base.convert{T<:AbstractPnum}(::Type{T}, x::Real) = _searchvalue(T, x)

# A NonEmptyPbound is stored as a packed binary unsigned integer where
# the upper and lower halves encode Pnums. This is a low-level type.
# For general purposes, the higher level Pbound type should be used,
# since it is capable of expressing the important idea of an empty
# Pbound
immutable NonEmptyPbound{T<:AbstractPnum} <: Number
  x::T
  y::T
  # Always store everything in the canonical way
  function NonEmptyPbound(x::T, y::T)
    iseverything(x, y) ? new(nextpnum(pninf(T)), pninf(T)) : new(x, y)
  end
end

unpack(npb::NonEmptyPbound) = npb.x, npb.y
NonEmptyPbound{T}(x::T, y::T) = NonEmptyPbound{T}(x, y)

immutable Pbound{T} <: Number
  isempty::Bool
  v::NonEmptyPbound{T}
end

Base.isempty(x::Pbound) = x.isempty
unpack(x::Pbound) = (isempty(x), unpack(x.v)...)

type Sopn{T<:AbstractPnum} <: Number
  s::IntSet
end

Sopn{T}(::Type{T}) = Sopn{T}(IntSet())
Sopn(itr) = reduce(union!, Sopn(eltype(itr)), itr)

# Define some useful promotions
Base.promote_rule{S<:AbstractPnum, T<:Real}(::Type{S}, ::Type{T}) = S
Base.promote_rule{S<:Pbound, T<:Real}(::Type{S}, ::Type{T}) = S
Base.promote_rule{S<:AbstractPnum}(::Type{Pbound{S}}, ::Type{S}) = Pbound{S}