function Base.parse(::Type{Pnum}, str)
  m = match(r"^(-?)(/?)(\d)$", str)
  m != nothing && return parseexact(Pnum, m)
  m = match(r"^([\[\(])\s*(-?)(/?)(\d)\s*,\s*(-?)(/?)(\d)\s*([\]\)])$", str)
  m != nothing && return parseinterval(Pnum, m)
  throw(ArgumentError("Improperly formatted Pnum"))
end

macro pn_str(str)
  parse(Pnum, str)
end

function _frompieces(negative, reciprocal, value)
  reciprocal && value == 0 && return pninf

  value = negative ? -value : value
  value = reciprocal ? 1//value : value
  i = searchsorted(exacts, value)
  first(i) != last(i) && throw(InexactError())
  return rawpnum(UInt8(mod(2*first(i) - 4, 8)))
end

function parsefirst(closed, negative, reciprocal, value)
  x = _frompieces(negative, reciprocal, value)
  return closed ? x : next(x)
end

function parsesecond(closed, negative, reciprocal, value)
  x = _frompieces(negative, reciprocal, value)
  return closed ? x : prev(x)
end

function parseexact(::Type{Pnum}, match)
 return _frompieces(
    match.captures[1] == "-",
    match.captures[2] == "/",
    parse(Int, match.captures[3])
  )
end

function parseinterval(::Type{Pnum}, match)
  x1 = parsefirst(
    match.captures[1] == "[",
    match.captures[2] == "-",
    match.captures[3] == "/",
    parse(Int, match.captures[4])
  )
  x2 = parsesecond(
    match.captures[8] == "]",
    match.captures[5] == "-",
    match.captures[6] == "/",
    parse(Int, match.captures[7])
  )
  if x1.v != x2.v
    throw(ArgumentError("Improperly formatted Pnum"))
  end
  return x1
end

_str(x::Pnum) = isinf(x) ? "/0" : string(num(exactvalue(x)))

function Base.show(io::IO, x::Pnum)
  if (isexact(x))
    print(io, "pn\"", _str(x), "\"")
  else
    print(io, "pn\"", "(", _str(prev(x)), ", ", _str(next(x)), ")\"")
  end
end

function Base.parse(::Type{Pbound}, str)
  str == "empty" && return pbempty
  str == "everything" && return pbeverything
  m = match(r"^(-?)(/?)(\d)$", str)
  m != nothing && return parseexact(Pbound, m)
  m = match(r"^([\[\(])\s*(-?)(/?)(\d)\s*,\s*(-?)(/?)(\d)\s*([\]\)])$", str)
  m != nothing && return parseinterval(Pbound, m)
  throw(ArgumentError("Improperly formatted Pbound"))
end

macro pb_str(str)
  parse(Pbound, str)
end

function parseexact(::Type{Pbound}, match)
  x = parseexact(Pnum, match)
  return Pbound(x, x)
end

function parseinterval(::Type{Pbound}, match)
  x1 = parsefirst(
    match.captures[1] == "[",
    match.captures[2] == "-",
    match.captures[3] == "/",
    parse(Int, match.captures[4])
  )
  x2 = parsesecond(
    match.captures[8] == "]",
    match.captures[5] == "-",
    match.captures[6] == "/",
    parse(Int, match.captures[7])
  )
  return Pbound(x1, x2)
end

function Base.show(io::IO, x::Pbound)
  empty, x1, x2 = unpack(x)

  if empty
    print(io, "pb\"empty\"")
  elseif iseverything(x)
    print(io, "pb\"everything\"")
  elseif isexact(x)
    print(io, "pb\"", _str(x1), "\"")
  else
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
