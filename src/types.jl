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
const pn3nvalues = 8*length(pn3exacts)
exacts(::Type{Pnum3}) = pn3exacts
pnnvalues(::Type{Pnum3}) = pn3nvalues
pnmask(::Type{Pnum3}) = convert(storagetype(Pnum3), pnnvalues(Pnum3) - 1)
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

immutable Pnum4 <: AbstractPnum
  v::UInt8
  Pnum4(b::Bitmask{UInt8}) = new(pnmod(Pnum4, b.v))
end

storagetype(::Type{Pnum4}) = UInt8
const pn4exacts = [1//1, 2//1]
const pn4nvalues = 8*length(pn4exacts)
exacts(::Type{Pnum4}) = pn4exacts
pnnvalues(::Type{Pnum4}) = pn4nvalues
pnmask(::Type{Pnum4}) = convert(storagetype(Pnum4), pnnvalues(Pnum4) - 1)
rawpnum(::Type{Pnum4}, x::storagetype(Pnum4)) = Pnum4(Bitmask(x))
pnmod(::Type{Pnum4}, x::storagetype(Pnum4)) = x & pnmask(Pnum4)

macro pn4_str(str)
  parse(Pnum4, str)
end

pnprefix(x::Pnum4) = "pn4"

macro pb4_str(str)
  parse(Pbound{Pnum4}, str)
end

pbprefix(x::Pbound{Pnum4}) = "pb4"

typealias Pbound4 Pbound{Pnum4}

immutable Pnum8 <: AbstractPnum
  v::UInt8
  Pnum8(b::Bitmask{UInt8}) = new(pnmod(Pnum8, b.v))
end

storagetype(::Type{Pnum8}) = UInt8
const pn8exacts = vcat([2^n*[4//4, 5//4, 6//4, 7//4] for n in 0:7]...)
const pn8nvalues = 8*length(pn8exacts)
exacts(::Type{Pnum8}) = pn8exacts
pnnvalues(::Type{Pnum8}) = pn8nvalues
pnmask(::Type{Pnum8}) = convert(storagetype(Pnum8), pnnvalues(Pnum8) - 1)
rawpnum(::Type{Pnum8}, x::storagetype(Pnum8)) = Pnum8(Bitmask(x))
pnmod(::Type{Pnum8}, x::storagetype(Pnum8)) = x & pnmask(Pnum8)

macro pn8_str(str)
  parse(Pnum8, str)
end

pnprefix(x::Pnum8) = "pn8"

macro pb8_str(str)
  parse(Pbound{Pnum8}, str)
end

pbprefix(x::Pbound{Pnum8}) = "pb8"

typealias Pbound8 Pbound{Pnum8}

# Define some useful promotions
Base.promote_rule{S<:AbstractPnum, T<:Real}(::Type{S}, ::Type{T}) = S
Base.promote_rule{S<:Pbound, T<:Real}(::Type{S}, ::Type{T}) = S
Base.promote_rule{S<:AbstractPnum}(::Type{Pbound{S}}, ::Type{S}) = Pbound{S}