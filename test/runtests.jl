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

const allpbounds = let accum = Pbound[pb3"empty"]
  for x in eachpnum(pb3"everything"), y in eachpnum(pb3"everything")
    push!(accum, Pbound(x, y))
  end
  accum
end

# Test arithmetic over bounds by checking that it is consistent with
# Sopn arithmetic.
for x1 in allpbounds, x2 in allpbounds
  @test Pnums.Sopn(x1 + x2) == Pnums.Sopn(x1) + Pnums.Sopn(x2)
  @test Pnums.Sopn(x1 - x2) == Pnums.Sopn(x1) - Pnums.Sopn(x2)
  @test Pnums.Sopn(x1 * x2) == Pnums.Sopn(x1) * Pnums.Sopn(x2)
  @test Pnums.Sopn(x1 / x2) == Pnums.Sopn(x1) / Pnums.Sopn(x2)
  @test (x1 == x2) == (Pnums.Sopn(x1) == Pnums.Sopn(x2))
end

@test bisectvalue(x->x*(x-1)*(x+1), pn3"0") == [ pb3"-1", pb3"0", pb3"1" ]
@test bisectvalue(x->x*x + 1, pn3"0") == Pbound[]
@test bisectvalue(x->x, pn3"/0") == [ pb3"/0" ]
@test bisectvalue(x->(1+x)/(1-x), pn3"0") == [ pb3"-1", pb3"/0" ]
@test bisectvalue(x->(1+x)/(1-x), pn3"/0") == [ pb3"1", pb3"/0" ]
@test bisectvalue(x->x*x, Pnum3(2)) == [ pb3"(/0, -1)", pb3"(1, /0)" ]
@test bisectvalue(x->pb3"(-1, 1)", pn3"0") == [ pb3"everything" ]

@test exp(pn3"0") == pb3"1"
@test exp(pn3"(0, 1)") == pb3"(1, /0)"
@test exp(pn3"1") == pb3"(1, /0)"
@test exp(pn3"(1, /0)") == pb3"(1, /0)"
@test exp(pn3"/0") == pb3"[0, /0]"
@test exp(pn3"(/0, -1)") == pb3"(0, 1)"
@test exp(pn3"-1") == pb3"(0, 1)"
@test exp(pn3"(-1, 0)") == pb3"(0, 1)"

for x1 in allpbounds
  @test Pnums.Sopn(exp(x1)) == exp(Pnums.Sopn(x1))
end
