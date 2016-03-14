using Pnums
using Base.Test

# Conversion from reals
@test Pnum3(0) == pn3"0"
@test Pnum3(1) == pn3"1"
@test Pnum3(Inf) == pn3"/0"
@test Pnum3(-1) == pn3"-1"
@test Pnum3(0.5) == pn3"(0, 1)"
@test Pnum3(10) == pn3"(1, /0)"
@test Pnum3(-10) == pn3"(/0, -1)"
@test Pnum3(-0.5) ==  pn3"(-1, 0)"

@test Pbound3(0) == pb3"0"
@test Pbound3(1) == pb3"1"
@test Pbound3(Inf) == pb3"/0"
@test Pbound3(-1) == pb3"-1"
@test Pbound3(0.5) == pb3"(0, 1)"
@test Pbound3(10) == pb3"(1, /0)"
@test Pbound3(-10) == pb3"(/0, -1)"
@test Pbound3(-0.5) ==  pb3"(-1, 0)"

# Negating Pnums
@test -pn3"0" == pn3"0"
@test -pn3"(0, 1)" == pn3"(-1, 0)"
@test -pn3"1" == pn3"-1"
@test -pn3"(1, /0)" == pn3"(/0, -1)"
@test -pn3"/0" == pn3"/0"
@test -pn3"(/0, -1)" == pn3"(1, /0)"
@test -pn3"-1" == pn3"1"
@test -pn3"(-1, 0)" == pn3"(0, 1)"

# Reciprocating Pnums
@test inv(pn3"0") == pn3"/0"
@test inv(pn3"(0, 1)") == pn3"(1, /0)"
@test inv(pn3"1") == pn3"1"
@test inv(pn3"(1, /0)") == pn3"(0, 1)"
@test inv(pn3"/0") == pn3"0"
@test inv(pn3"(/0, -1)") == pn3"(-1, 0)"
@test inv(pn3"-1") == pn3"-1"
@test inv(pn3"(-1, 0)") == pn3"(/0, -1)"

# isexact
@test Pnums.isexact(pn3"0") == true
@test Pnums.isexact(pn3"(0, 1)") == false
@test Pnums.isexact(pn3"1") == true
@test Pnums.isexact(pn3"(1, /0)") == false
@test Pnums.isexact(pn3"/0") == true
@test Pnums.isexact(pn3"(/0, -1)") == false
@test Pnums.isexact(pn3"-1") == true
@test Pnums.isexact(pn3"(-1, 0)") == false

# next
@test Pnums.nextpnum(pn3"0") == pn3"(0, 1)"
@test Pnums.nextpnum(pn3"(0, 1)") == pn3"1"
@test Pnums.nextpnum(pn3"1") == pn3"(1, /0)"
@test Pnums.nextpnum(pn3"(1, /0)") == pn3"/0"
@test Pnums.nextpnum(pn3"/0") == pn3"(/0, -1)"
@test Pnums.nextpnum(pn3"(/0, -1)") == pn3"-1"
@test Pnums.nextpnum(pn3"-1") == pn3"(-1, 0)"
@test Pnums.nextpnum(pn3"(-1, 0)") == pn3"0"

# prev
@test Pnums.prevpnum(pn3"0") == pn3"(-1, 0)"
@test Pnums.prevpnum(pn3"(0, 1)") == pn3"0"
@test Pnums.prevpnum(pn3"1") == pn3"(0, 1)"
@test Pnums.prevpnum(pn3"(1, /0)") == pn3"1"
@test Pnums.prevpnum(pn3"/0") == pn3"(1, /0)"
@test Pnums.prevpnum(pn3"(/0, -1)") == pn3"/0"
@test Pnums.prevpnum(pn3"-1") == pn3"(/0, -1)"
@test Pnums.prevpnum(pn3"(-1, 0)") == pn3"-1"

for x1 in eachpnum(pb3"everything"), x2 in eachpnum(pb3"everything")
  x = Pbound(x1, x2)
  @test -(-x) == x
  if pn3"0" in x
    @test pn3"0" in -x
  end
  if pn3"/0" in x
    @test pn3"/0" in -x
  end
  if pn3"1" in x
    @test pn3"-1" in -x
  end
  if pn3"-1" in x
    @test pn3"1" in -x
  end

  @test inv(inv(x)) == x
  if pn3"0" in x
    @test pn3"/0" in inv(x)
  end
  if pn3"/0" in x
    @test pn3"0" in inv(x)
  end
  if pn3"1" in x
    @test pn3"1" in inv(x)
  end
  if pn3"-1" in x
    @test pn3"-1" in inv(x)
  end
end

for x1 in eachpnum(pb3"everything"), x2 in eachpnum(pb3"everything")
  x = Pbound(x1, x2)
  for x3 in x
    @test !(x3 in complement(x))
  end
end

@test pn3"0" + pn3"0" == pb3"0"
@test pn3"0" + pn3"/0" == pb3"/0"
@test pn3"/0" + pn3"0" == pb3"/0"
@test pn3"/0" + pn3"/0" == pb3"everything"

for x1 in eachpnum(pb3"everything")
  if !isinf(x1)
    x1 + pn3"/0" == pn3"/0"
  end
end

@test pn3"(0, 1)" + pn3"(0, 1)" == pb3"(0, /0)"
@test pn3"(0, 1)" + pn3"(1, /0)" == pb3"(1, /0)"
@test pn3"(0, 1)" + pn3"(/0, -1)" == pb3"(/0, 0)"
@test pn3"(0, 1)" + pn3"(-1, 0)" == pb3"(-1, 1)"
@test pn3"(1, /0)" + pn3"(1, /0)" == pb3"(1, /0)"
@test pn3"(1, /0)" + pn3"(/0, -1)" == pb3"(/0, /0)"
@test pn3"(1, /0)" + pn3"(-1, 0)" == pb3"(0, /0)"
@test pn3"(/0, -1)" + pn3"(/0, -1)" == pb3"(/0, -1)"
@test pn3"(/0, -1)" + pn3"(-1, 0)" == pb3"(/0, -1)"
@test pn3"(-1, 0)" + pn3"(-1, 0)" == pb3"(/0, 0)"

@test pn3"0" * pn3"0" == pb3"0"
@test pn3"0" * pn3"/0" == pb3"everything"
@test pn3"/0" * pn3"0" == pb3"everything"
@test pn3"/0" * pn3"/0" == pb3"/0"

for x1 in eachpnum(pb3"everything")
  if !isinf(x1) && !Pnums.iszero(x1)
    x1*pn3"0" == pn3"0"
    x1*pn3"/0" == pn3"/0"
  end
end

@test pn3"(0, 1)" * pn3"(0, 1)" == pb3"(0, 1)"
@test pn3"(0, 1)" * pn3"(1, /0)" == pb3"(0, /0)"
@test pn3"(0, 1)" * pn3"(/0, -1)" == pb3"(/0, 0)"
@test pn3"(0, 1)" * pn3"(-1, 0)" == pb3"(-1, 0)"
@test pn3"(1, /0)" * pn3"(1, /0)" == pb3"(1, /0)"
@test pn3"(1, /0)" * pn3"(/0, -1)" == pb3"(/0, -1)"
@test pn3"(1, /0)" * pn3"(-1, 0)" == pb3"(/0, 0)"
@test pn3"(/0, -1)" * pn3"(/0, -1)" == pb3"(1, /0)"
@test pn3"(/0, -1)" * pn3"(-1, 0)" == pb3"(0, /0)"
@test pn3"(-1, 0)" * pn3"(-1, 0)" == pb3"(0, 1)"

@test pn3"0" - pn3"0" == pb3"0"
@test pn3"0" - pn3"/0" == pb3"/0"
@test pn3"/0" - pn3"0" == pb3"/0"
@test pn3"/0" - pn3"/0" == pb3"everything"

@test pn3"(0, 1)" - pn3"(0, 1)" == pb3"(-1, 1)"
@test pn3"(0, 1)" - pn3"(1, /0)" == pb3"(/0, 0)"
@test pn3"(0, 1)" - pn3"(/0, -1)" == pb3"(1, /0)"
@test pn3"(0, 1)" - pn3"(-1, 0)" == pb3"(0, /0)"
@test pn3"(1, /0)" - pn3"(1, /0)" == pb3"(/0, /0)"
@test pn3"(1, /0)" - pn3"(/0, -1)" == pb3"(1, /0)"
@test pn3"(1, /0)" - pn3"(-1, 0)" == pb3"(1, /0)"
@test pn3"(/0, -1)" - pn3"(/0, -1)" == pb3"(/0, /0)"
@test pn3"(/0, -1)" - pn3"(-1, 0)" == pb3"(/0, 0)"
@test pn3"(-1, 0)" - pn3"(-1, 0)" == pb3"(-1, 1)"

@test pn3"0" / pn3"0" == pb3"everything"
@test pn3"0" / pn3"/0" == pb3"0"
@test pn3"/0" / pn3"0" == pb3"/0"
@test pn3"/0" / pn3"/0" == pb3"everything"

@test pn3"(0, 1)" / pn3"(0, 1)" == pb3"(0, /0)"
@test pn3"(0, 1)" / pn3"(1, /0)" == pb3"(0, 1)"
@test pn3"(0, 1)" / pn3"(/0, -1)" == pb3"(-1, 0)"
@test pn3"(0, 1)" / pn3"(-1, 0)" == pb3"(/0, 0)"
@test pn3"(1, /0)" / pn3"(1, /0)" == pb3"(0, /0)"
@test pn3"(1, /0)" / pn3"(/0, -1)" == pb3"(/0, 0)"
@test pn3"(1, /0)" / pn3"(-1, 0)" == pb3"(/0, -1)"
@test pn3"(/0, -1)" / pn3"(/0, -1)" == pb3"(0, /0)"
@test pn3"(/0, -1)" / pn3"(-1, 0)" == pb3"(1, /0)"
@test pn3"(-1, 0)" / pn3"(-1, 0)" == pb3"(0, /0)"

const allpb3 = let accum = Pbound[pb3"empty"]
  for x in eachpnum(pb3"everything"), y in eachpnum(pb3"everything")
    push!(accum, Pbound(x, y))
  end
  accum
end

const allpb4 = let accum = Pbound[pb4"empty"]
  for x in eachpnum(pb4"everything"), y in eachpnum(pb4"everything")
    push!(accum, Pbound(x, y))
  end
  accum
end

# Test arithmetic over bounds by checking that it is consistent with
# Sopn arithmetic.
for x1 in allpb3, x2 in allpb3
  @test Pnums.Sopn(x1 + x2) == Pnums.Sopn(x1) + Pnums.Sopn(x2)
  @test Pnums.Sopn(x1 - x2) == Pnums.Sopn(x1) - Pnums.Sopn(x2)
  @test Pnums.Sopn(x1 * x2) == Pnums.Sopn(x1) * Pnums.Sopn(x2)
  @test Pnums.Sopn(x1 / x2) == Pnums.Sopn(x1) / Pnums.Sopn(x2)
  @test (x1 == x2) == (Pnums.Sopn(x1) == Pnums.Sopn(x2))
end

@test exp(pn3"0") == pb3"1"
@test exp(pn3"(0, 1)") == pb3"(1, /0)"
@test exp(pn3"1") == pb3"(1, /0)"
@test exp(pn3"(1, /0)") == pb3"(1, /0)"
@test exp(pn3"/0") == pb3"[0, /0]"
@test exp(pn3"(/0, -1)") == pb3"(0, 1)"
@test exp(pn3"-1") == pb3"(0, 1)"
@test exp(pn3"(-1, 0)") == pb3"(0, 1)"

for x in allpb3
  @test Pnums.Sopn(exp(x)) == exp(Pnums.Sopn(x))
end

for x in allpb4
  @test Pnums.Sopn(exp(x)) == exp(Pnums.Sopn(x))
end

@test bisectvalue(x->x*(x-1)*(x+1), pn3"0") == [ pb3"-1", pb3"0", pb3"1" ]
@test bisectvalue(x->x*x + 1, pn3"0") == Pbound[]
@test bisectvalue(x->x, pn3"/0") == [ pb3"/0" ]
@test bisectvalue(x->(1+x)/(1-x), pn3"0") == [ pb3"-1", pb3"/0" ]
@test bisectvalue(x->(1+x)/(1-x), pn3"/0") == [ pb3"1", pb3"/0" ]
@test bisectvalue(x->x*x, Pnum3(2)) == [ pb3"(/0, -1)", pb3"(1, /0)" ]
@test bisectvalue(x->pb3"(-1, 1)", pn3"0") == [ pb3"everything" ]

@test bisectvalue(x->x*x, pn8"2") == [
  pb8"(-3/2, -5/4)",
  pb8"(5/4, 3/2)"
]

@test bisectvalue(x->x*x, pn16"2") == [
  pb16"(-363/256, -181/128)",
  pb16"(181/128, 363/256)"
]

# This one is interesting because it shows how the dependency problem
# can lead to imprecise solutions and "false positive" solutions.
#
# Notice that prev(pn8"/0") - prev(pn8"/0") = (/0, /0), i.e. the set of
# all finite pnums. It is also a fixed point of multiplication. This
# means it will show up as a solution whenever we add or subtract
# non-constant monomials.
bisectvalue(x->x*x*x - x, pn8"0") == [
  pb8"(/0, -224)",
  pb8"(-5/4, -4/5)",
  pb8"(-1/224, /224)",
  pb8"(4/5, 5/4)",
  pb8"(224, /0]"
]
