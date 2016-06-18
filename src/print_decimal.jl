# Functions for printing Pnums and Pbounds as decimals.
#
# Segregating from the rest of io because I'm not very certain about
# how to do this right--this is a quick hack so far.
function print_decimal(io::IO, x::AbstractPnum)
  if (isexact(x))
    print(io, Float32(exactvalue(x)))
  else
    left = Float32(exactvalue(prevpnum(x)))
    right = Float32(exactvalue(nextpnum(x)))
    print(io, "(", left, ", ", right, ")")
  end
end

print_decimal(x::AbstractPnum) = print_decimal(STDOUT, x)

function print_decimal(io::IO, x::Pbound)
  empty, x1, x2 = unpack(x)

  if empty
    print(io, "empty")
  elseif iseverything(x)
    print(io, "everything")
  elseif isexact(x)
    print(io, Float32(exactvalue(x1)))
  else
    if isexact(x1)
      print(io, "[", Float32(exactvalue(x1)))
    else
      print(io, "(", Float32(exactvalue(prevpnum(x1))))
    end

    print(io, ", ")

    if isexact(x2)
      print(io, Float32(exactvalue(x2)), "]")
    else
      print(io, Float32(exactvalue(nextpnum(x2))), ")")
    end

  end
end

print_decimal(x::Pbound) = print_decimal(STDOUT, x)
print_decimal(arr::AbstractArray) = for x in arr
  print_decimal(x)
  println()
end