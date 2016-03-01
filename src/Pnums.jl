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

# Store unums in a byte with 5 leading zeros
# Store ubounds in a byte
# Store SORNs in a byte
#
# Interesting that for these numbers, a ubound and a SORN take the same
# number of bytes to represent
immutable Pnum
  v::UInt8
  Pnum(v) = new(UInt8(v) & 0x07) # TODO magic 00000111 bitmask
end

Base.isfinite(x::Pnum) = x.v !== 0x04 # TODO magic number for infinity
isexact(x::Pnum) = (x.v & 0x01) == 0x00

function exactvalue(x::Pnum)
  if !isfinite(x)
    1//0
  else
    exacts[mod((x.v >> 1) + 2, 4)]
  end
end

_str(x::Pnum) = isfinite(x) ? string(num(exactvalue(x))) : "/0"

function Base.show(io::IO, x::Pnum)
  if (isexact(x))
    print(io, "pn\"", "[", _str(x), ", ", _str(x), "]\"")
  else
    print(io, "pn\"", "(", _str(prev(x)), ", ", _str(next(x)), ")\"")
  end
end

const pnumstrings = Dict(
  "0" => Pnum(0x00),
  "[0, 0]" => Pnum(0x00),
  "(0, 1)" => Pnum(0x01),
  "1" => Pnum(0x02),
  "[1, 1]" => Pnum(0x02),
  "(1, /0)" => Pnum(0x03),
  "/0" => Pnum(0x04),
  "[/0, /0]" => Pnum(0x04),
  "(/0, -1)" => Pnum(0x05),
  "-1" => Pnum(0x06),
  "[-1, -1]" => Pnum(0x06),
  "(-1, 0)" => Pnum(0x07)
)

macro pn_str(p)
  # TODO proper error message for improperly formatted Pnum
  # TODO less hardcoding of pnum input
  # TODO less strict whitespace in pnum input
  pnumstrings[p]
end

immutable Pbound
  v::UInt8
end

immutable Sorn
  v::UInt8
end

Base.(:-)(x::Pnum) = Pnum(-x.v)
# Rotate 90 degrees, negate, and rotate back 90 degrees
# TODO, 0x02 is a magic number for rotating 90 degrees
recip(x::Pnum) = Pnum(-(x.v + 0x02) - 0x02)

next(x::Pnum) = Pnum(x.v + 1)
prev(x::Pnum) = Pnum(x.v - 1)

# TODO, 3 is a magic number (the number of bits in our Pnums)
Pbound(x::Pnum, y::Pnum) = Pbound((x.v << 3) | y.v)
first(x::Pbound) = Pnum(x.v >> 3)
second(x::Pbound) = Pnum(x.v)
# TODO, 0xc0 is a magic number: "11000000", the first two bits of a byte
tag(x::Pbound) = Pbound(x.v & 0xc0)

Base.(:-)(x::Pbound) = Pbound(tag(x).v | Pbound(-second(x), -first(x)).v)
recip(x::Pbound) = Pbound(tag(x).v | Pbound(recip(second(x)), recip(first(x))).v)

# TODO 0xff is a magic number: "10000000", the complement bit
complement(x::Pbound) = Pbound(x.v $ 0xff)

# Arithmetic:
# Make tables for + and *. They will be 8x8 arrays of Pbounds.
# All arithmetic on "nothing" produces "nothing"
# Ways to produce "everything":
#   * /0 + /0
#   * 0*/0 or /0*0
#   * everything*(something except 0 or /0)
#   * everything + something
# Multiplying 0 or /0 by something (i.e. not nothing) produces 0 or /0

# Need to implement a way of coercing point values into Pnums. Binary search
# on exact values is probably the way to go.

# "Normalize" Pbounds:
#   Everything -> 01000000
#   Nothing    -> 11000000
#   first <= second

export Pnum, @pn_str, isexact, recip, Pbound, first, second

end
