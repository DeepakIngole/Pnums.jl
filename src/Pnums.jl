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

const exacts = [-1//1, 0//1, 1//1]

# Store unums in a byte with 5 leading zeros
# Store ubounds in a byte
# Store SOPNs in a byte
#
# Interesting that for these numbers, a ubound and a SOPN take the same
# number of bytes to represent
immutable Pnum
  v::UInt8
  Pnum(v) = new(UInt8(v) & 0x07) # TODO magic 00000111 bitmask
end

Base.isfinite(x::Pnum) = x.v != 0x04 # TODO magic number for infinity
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

Base.(:-)(x::Pnum) = Pnum(-x.v)
# Rotate 90 degrees, negate, and rotate back 90 degrees
# TODO, 0x02 is a magic number for rotating 90 degrees
recip(x::Pnum) = Pnum(-(x.v + 0x02) - 0x02)

# Next and prev move us clockwise around the stereographic circle
next(x::Pnum) = Pnum(x.v + 0x01)
prev(x::Pnum) = Pnum(x.v - 0x01)

immutable Pbound
  v::UInt8
end

# TODO, 3 is a magic number (the number of bits in our Pnums)
Pbound(x::Pnum, y::Pnum) = Pbound((x.v << 3) | y.v)
unpack(x::Pbound) = (Pnum(x.v >> 3), Pnum(x.v))
# TODO, 0xc0 is a magic number: "11000000", the first two bits of a byte
tag(x::Pbound) = Pbound(x.v & 0xc0)

isempty(x::Pbound) = (x.v & 0x80) == 0x80
function iseverything(x::Pbound)
  x1, x2 = unpack(x)
  mod(x1.v - x2.v, 0x08) == 0x01
end

const empty = Pbound(0x80)
const everything = Pbound(Pnum(0x00), Pnum(0xff))

function Base.show(io::IO, x::Pbound)
  if isempty(x)
    print(io, "pb\"empty\"")
  elseif iseverything(x)
    print(io, "pb\"everything\"")
  else
    x1, x2 = unpack(x)

    print(io, "pb\"")

    if isexact(x1)
      print(io, "[", _str(x1))
    else
      print(io, "(", _str(prev(x1)))
    end

    print(io, ", ")

    if isexact(x2)
      print(io, _str(x2), "]")
    else
      print(io, _str(next(x2)), ")")
    end

    print(io, "\"")
  end
end

function _parse(negative, reciprocal, value)
  if negative
    value = -value
  end
  if reciprocal && value == 0
    return pn"/0"
  else
    # TODO deal with other reciprocal cases
    # TODO deal with "in-between" values (probably by erroring)
    i = searchsortedfirst(exacts, value)
    return Pnum(UInt8(mod(2*i - 4, 8)))
  end
end

function parsefirst(closed, negative, reciprocal, value)
  x = _parse(negative, reciprocal, value)
  return closed ? x : next(x)
end

function parsesecond(closed, negative, reciprocal, value)
  x = _parse(negative, reciprocal, value)
  return closed ? x : prev(x)
end

macro pb_str(p)
  p == "empty" && return empty
  p == "everything" && return everything
  m = match(r"([\[\(])\s*(-?)(/?)(\d)\s*,\s*(-?)(/?)(\d)([\]\)])", p)
  x1 = parsefirst(
    m.captures[1] == "[",
    m.captures[2] == "-",
    m.captures[3] == "/",
    parse(Int, m.captures[4])
  )
  x2 = parsesecond(
    m.captures[8] == "]",
    m.captures[5] == "-",
    m.captures[6] == "/",
    parse(Int, m.captures[7])
  )
  Pbound(x1, x2)
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

# Need to implement a way of coercing point values into Pnums. Binary search
# on exact values is probably the way to go.

immutable Sopn
  v::UInt8
end

export Pnum, Pbound, @pn_str, @pb_str, isexact, recip

end
