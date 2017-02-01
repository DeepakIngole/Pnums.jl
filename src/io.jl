function Base.parse{T<:AbstractPnum}(::Type{T}, str)
  m = match(r"^(-?)(\d*)(?:/(\d+))?$", str)
  m != nothing && return parseexact(T, m)
  m = match(r"^([\[\(])\s*(-?)(\d*)(?:/(\d+))?\s*,\s*(-?)(\d*)(?:/(\d+))?\s*([\]\)])$", str)
  m != nothing && return parseinterval(T, m)
  throw(ArgumentError("Improperly formatted Pnum"))
end

function parsefirst{T}(::Type{T}, closed, value)
  out = T(value)
  out == value || throw(InexactError)
  return closed ? out : nextpnum(out)
end

function parsesecond{T}(::Type{T}, closed, value)
  out = T(value)
  out == value || throw(InexactError)
  return closed ? out : prevpnum(out)
end

function _frompieces(neg, top, bottom)
  num, den = 1, 1
  if top == ""
    num = 1
    den = parse(Int, bottom)
  else
    num = parse(Int, top)
    den = bottom == nothing ? 1 : parse(Int, bottom)
  end

  num = neg == "-" ? -num : num
  num//den
end

function parseexact{T<:AbstractPnum}(::Type{T}, match)
  value = _frompieces(match[1], match[2], match[3])
  out = T(value)
  out == value || throw(InexactError)
  out
end

function parseinterval{T<:AbstractPnum}(::Type{T}, match)
  x1 = parsefirst(
    T,
    match.captures[1] == "[",
    _frompieces(match[2], match[3], match[4])
  )
  x2 = parsesecond(
    T,
    match.captures[8] == "]",
    _frompieces(match[5], match[6], match[7])
  )
  if x1.v != x2.v
    throw(ArgumentError("Improperly formatted Pnum"))
  end
  return x1
end

if !isdefined(Base, :denominator) # Julia issue #19233
  const denominator = Base.den
  const numerator = Base.num
end

function _str(x::AbstractPnum)
  isinf(x) && return "/0"
  v = exactvalue(x)
  denominator(v) == 1 && return string(numerator(v))
  numerator(v) == 1 && return string("/", denominator(v))
  string(numerator(v), "/", denominator(v))
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
  m = match(r"^(-?)(\d*)(?:/(\d+))?$", str)
  m != nothing && return parseexact(T, m)
  m = match(r"^([\[\(])\s*(-?)(\d*)(?:/(\d+))?\s*,\s*(-?)(\d*)(?:/(\d+))?\s*([\]\)])$", str)
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
    _frompieces(match[2], match[3], match[4])
  )
  x2 = parsesecond(
    T,
    match.captures[8] == "]",
    _frompieces(match[5], match[6], match[7])
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
