function Base.parse{T<:AbstractPnum}(::Type{T}, str)
  m = match(r"^(-?)(/?)(\d)$", str)
  m != nothing && return parseexact(T, m)
  m = match(r"^([\[\(])\s*(-?)(/?)(\d)\s*,\s*(-?)(/?)(\d)\s*([\]\)])$", str)
  m != nothing && return parseinterval(T, m)
  throw(ArgumentError("Improperly formatted Pnum"))
end

function _frompieces{T<:AbstractPnum}(::Type{T}, negative, reciprocal, value)
  reciprocal && value == 0 && return pninf(T)

  value = negative ? -value : value
  value = reciprocal ? 1//value : value
  out = T(value)
  (out == value) || throw(InexactError())
  return out
end

function parsefirst{T}(::Type{T}, closed, negative, reciprocal, value)
  x = _frompieces(T, negative, reciprocal, value)
  return closed ? x : nextpnum(x)
end

function parsesecond{T}(::Type{T}, closed, negative, reciprocal, value)
  x = _frompieces(T, negative, reciprocal, value)
  return closed ? x : prevpnum(x)
end

function parseexact{T<:AbstractPnum}(::Type{T}, match)
 return _frompieces(
    T,
    match.captures[1] == "-",
    match.captures[2] == "/",
    parse(Int, match.captures[3])
  )
end

function parseinterval{T<:AbstractPnum}(::Type{T}, match)
  x1 = parsefirst(
    T,
    match.captures[1] == "[",
    match.captures[2] == "-",
    match.captures[3] == "/",
    parse(Int, match.captures[4])
  )
  x2 = parsesecond(
    T,
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

function _str(x::AbstractPnum)
  isinf(x) && return "/0"
  v = exactvalue(x)
  den(v) == 1 && return string(num(v))
  num(v) == 1 && return string("/", den(v))
  string(num(v), "/", den(v))
end

function Base.show(io::IO, x::AbstractPnum)
  if (isexact(x))
    print(io, pnprefix(x), "\"", _str(x), "\"")
  else
    print(io, pnprefix(x), "\"", "(", _str(prevpnum(x)), ", ", _str(nextpnum(x)), ")\"")
  end
end

function Base.parse{T<:Pbound}(::Type{T}, str)
  str == "empty" && return pbempty(T)
  str == "everything" && return pbeverything(T)
  m = match(r"^(-?)(/?)(\d)$", str)
  m != nothing && return parseexact(T, m)
  m = match(r"^([\[\(])\s*(-?)(/?)(\d)\s*,\s*(-?)(/?)(\d)\s*([\]\)])$", str)
  m != nothing && return parseinterval(T, m)
  throw(ArgumentError("Improperly formatted Pbound"))
end

function parseexact{T<:AbstractPnum}(::Type{Pbound{T}}, match)
  x = parseexact(T, match)
  return Pbound(x, x)
end

function parseinterval{T<:AbstractPnum}(::Type{Pbound{T}}, match)
  x1 = parsefirst(
    T,
    match.captures[1] == "[",
    match.captures[2] == "-",
    match.captures[3] == "/",
    parse(Int, match.captures[4])
  )
  x2 = parsesecond(
    T,
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
    print(io, pbprefix(x), "\"empty\"")
  elseif iseverything(x)
    print(io, pbprefix(x), "\"everything\"")
  elseif isexact(x)
    print(io, pbprefix(x), "\"", _str(x1), "\"")
  else
    print(io, pbprefix(x), "\"")

    if isexact(x1)
      print(io, "[", _str(x1))
    else
      print(io, "(", _str(prevpnum(x1)))
    end

    print(io, ", ")

    if isexact(x2)
      print(io, _str(x2), "]")
    else
      print(io, _str(nextpnum(x2)), ")")
    end

    print(io, "\"")
  end
end
