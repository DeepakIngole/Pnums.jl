# Used for indirection purposes to allow Pnum(x::Integer) to do
# conversion by value instead of bit representation, while still having
# a way to construct a Pnum from raw bits.
immutable Bitmask{T<:Integer}
  v::T
end

abstract AbstractPnum <: Number

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

immutable Pnum3 <: AbstractPnum
  v::UInt8
  Pnum3(b::Bitmask{UInt8}) = new(pnmod(Pnum3, b.v))
end

storagetype(::Type{Pnum3}) = UInt8
const pn3exacts = [1//1]
exacts(::Type{Pnum3}) = pn3exacts
pnnvalues(::Type{Pnum3}) = convert(storagetype(Pnum3), 8)
pnmask(::Type{Pnum3}) = pnnvalues(Pnum3) - one(storagetype(Pnum3))
rawpnum(::Type{Pnum3}, x::storagetype(Pnum3)) = Pnum3(Bitmask(x))
pnmod(::Type{Pnum3}, x::storagetype(Pnum3)) = x & pnmask(Pnum3)

macro pn3_str(str)
  parse(Pnum3, str)
end

pnprefix(x::Pnum3) = "pn3"

macro pb3_str(str)
  parse(Pbound{Pnum3}, str)
end

pbprefix(x::Pbound{Pnum3}) = "pb3"

typealias Pbound3 Pbound{Pnum3}

# Define some useful promotions
Base.promote_rule{S<:AbstractPnum, T<:Real}(::Type{S}, ::Type{T}) = S
Base.promote_rule{S<:Pbound, T<:Real}(::Type{S}, ::Type{T}) = S
Base.promote_rule{S<:AbstractPnum}(::Type{Pbound{S}}, ::Type{S}) = Pbound{S}