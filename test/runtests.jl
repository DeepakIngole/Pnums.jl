using Pnums
using Base.Test

# Conversion from reals
@test Pnum(0) == pn"0"
@test Pnum(1) == pn"1"
@test Pnum(Inf) == pn"/0"
@test Pnum(-1) == pn"-1"
@test Pnum(0.5) == pn"(0, 1)"
@test Pnum(10) == pn"(1, /0)"
@test Pnum(-10) == pn"(/0, -1)"
@test Pnum(-0.5) ==  pn"(-1, 0)"

@test Pbound(0) == pb"0"
@test Pbound(1) == pb"1"
@test Pbound(Inf) == pb"/0"
@test Pbound(-1) == pb"-1"
@test Pbound(0.5) == pb"(0, 1)"
@test Pbound(10) == pb"(1, /0)"
@test Pbound(-10) == pb"(/0, -1)"
@test Pbound(-0.5) ==  pb"(-1, 0)"

# Negating Pnums
@test -pn"0" == pn"0"
@test -pn"(0, 1)" == pn"(-1, 0)"
@test -pn"1" == pn"-1"
@test -pn"(1, /0)" == pn"(/0, -1)"
@test -pn"/0" == pn"/0"
@test -pn"(/0, -1)" == pn"(1, /0)"
@test -pn"-1" == pn"1"
@test -pn"(-1, 0)" == pn"(0, 1)"

# Reciprocating Pnums
@test inv(pn"0") == pn"/0"
@test inv(pn"(0, 1)") == pn"(1, /0)"
@test inv(pn"1") == pn"1"
@test inv(pn"(1, /0)") == pn"(0, 1)"
@test inv(pn"/0") == pn"0"
@test inv(pn"(/0, -1)") == pn"(-1, 0)"
@test inv(pn"-1") == pn"-1"
@test inv(pn"(-1, 0)") == pn"(/0, -1)"

# isexact
@test Pnums.isexact(pn"0") == true
@test Pnums.isexact(pn"(0, 1)") == false
@test Pnums.isexact(pn"1") == true
@test Pnums.isexact(pn"(1, /0)") == false
@test Pnums.isexact(pn"/0") == true
@test Pnums.isexact(pn"(/0, -1)") == false
@test Pnums.isexact(pn"-1") == true
@test Pnums.isexact(pn"(-1, 0)") == false

# next
@test Pnums.next(pn"0") == pn"(0, 1)"
@test Pnums.next(pn"(0, 1)") == pn"1"
@test Pnums.next(pn"1") == pn"(1, /0)"
@test Pnums.next(pn"(1, /0)") == pn"/0"
@test Pnums.next(pn"/0") == pn"(/0, -1)"
@test Pnums.next(pn"(/0, -1)") == pn"-1"
@test Pnums.next(pn"-1") == pn"(-1, 0)"
@test Pnums.next(pn"(-1, 0)") == pn"0"

# prev
@test Pnums.prev(pn"0") == pn"(-1, 0)"
@test Pnums.prev(pn"(0, 1)") == pn"0"
@test Pnums.prev(pn"1") == pn"(0, 1)"
@test Pnums.prev(pn"(1, /0)") == pn"1"
@test Pnums.prev(pn"/0") == pn"(1, /0)"
@test Pnums.prev(pn"(/0, -1)") == pn"/0"
@test Pnums.prev(pn"-1") == pn"(/0, -1)"
@test Pnums.prev(pn"(-1, 0)") == pn"-1"

for x1 in eachpnum(pb"everything"), x2 in eachpnum(pb"everything")
  x = Pbound(x1, x2)
  @test -(-x) == x
  if pn"0" in x
    @test pn"0" in -x
  end
  if pn"/0" in x
    @test pn"/0" in -x
  end
  if pn"1" in x
    @test pn"-1" in -x
  end
  if pn"-1" in x
    @test pn"1" in -x
  end

  @test inv(inv(x)) == x
  if pn"0" in x
    @test pn"/0" in inv(x)
  end
  if pn"/0" in x
    @test pn"0" in inv(x)
  end
  if pn"1" in x
    @test pn"1" in inv(x)
  end
  if pn"-1" in x
    @test pn"-1" in inv(x)
  end
end

for x1 in eachpnum(pb"everything"), x2 in eachpnum(pb"everything")
  x = Pbound(x1, x2)
  for x3 in x
    @test !(x3 in complement(x))
  end
end

@test pn"0" + pn"0" == pb"0"
@test pn"0" + pn"/0" == pb"/0"
@test pn"/0" + pn"0" == pb"/0"
@test pn"/0" + pn"/0" == pb"everything"

for x1 in eachpnum(pb"everything")
  if !isinf(x1)
    x1 + pn"/0" == pn"/0"
  end
end

@test pn"(0, 1)" + pn"(0, 1)" == pb"(0, /0)"
@test pn"(0, 1)" + pn"(1, /0)" == pb"(1, /0)"
@test pn"(0, 1)" + pn"(/0, -1)" == pb"(/0, 0)"
@test pn"(0, 1)" + pn"(-1, 0)" == pb"(-1, 1)"
@test pn"(1, /0)" + pn"(1, /0)" == pb"(1, /0)"
@test pn"(1, /0)" + pn"(/0, -1)" == pb"(/0, /0)"
@test pn"(1, /0)" + pn"(-1, 0)" == pb"(0, /0)"
@test pn"(/0, -1)" + pn"(/0, -1)" == pb"(/0, -1)"
@test pn"(/0, -1)" + pn"(-1, 0)" == pb"(/0, -1)"
@test pn"(-1, 0)" + pn"(-1, 0)" == pb"(/0, 0)"

@test pn"0" * pn"0" == pb"0"
@test pn"0" * pn"/0" == pb"everything"
@test pn"/0" * pn"0" == pb"everything"
@test pn"/0" * pn"/0" == pb"/0"

for x1 in eachpnum(pb"everything")
  if !isinf(x1) && !Pnums.iszero(x1)
    x1*pn"0" == pn"0"
    x1*pn"/0" == pn"/0"
  end
end

@test pn"(0, 1)" * pn"(0, 1)" == pb"(0, 1)"
@test pn"(0, 1)" * pn"(1, /0)" == pb"(0, /0)"
@test pn"(0, 1)" * pn"(/0, -1)" == pb"(/0, 0)"
@test pn"(0, 1)" * pn"(-1, 0)" == pb"(-1, 0)"
@test pn"(1, /0)" * pn"(1, /0)" == pb"(1, /0)"
@test pn"(1, /0)" * pn"(/0, -1)" == pb"(/0, -1)"
@test pn"(1, /0)" * pn"(-1, 0)" == pb"(/0, 0)"
@test pn"(/0, -1)" * pn"(/0, -1)" == pb"(1, /0)"
@test pn"(/0, -1)" * pn"(-1, 0)" == pb"(0, /0)"
@test pn"(-1, 0)" * pn"(-1, 0)" == pb"(0, 1)"

@test pn"0" - pn"0" == pb"0"
@test pn"0" - pn"/0" == pb"/0"
@test pn"/0" - pn"0" == pb"/0"
@test pn"/0" - pn"/0" == pb"everything"

@test pn"(0, 1)" - pn"(0, 1)" == pb"(-1, 1)"
@test pn"(0, 1)" - pn"(1, /0)" == pb"(/0, 0)"
@test pn"(0, 1)" - pn"(/0, -1)" == pb"(1, /0)"
@test pn"(0, 1)" - pn"(-1, 0)" == pb"(0, /0)"
@test pn"(1, /0)" - pn"(1, /0)" == pb"(/0, /0)"
@test pn"(1, /0)" - pn"(/0, -1)" == pb"(1, /0)"
@test pn"(1, /0)" - pn"(-1, 0)" == pb"(1, /0)"
@test pn"(/0, -1)" - pn"(/0, -1)" == pb"(/0, /0)"
@test pn"(/0, -1)" - pn"(-1, 0)" == pb"(/0, 0)"
@test pn"(-1, 0)" - pn"(-1, 0)" == pb"(-1, 1)"

@test pn"0" / pn"0" == pb"everything"
@test pn"0" / pn"/0" == pb"0"
@test pn"/0" / pn"0" == pb"/0"
@test pn"/0" / pn"/0" == pb"everything"

@test pn"(0, 1)" / pn"(0, 1)" == pb"(0, /0)"
@test pn"(0, 1)" / pn"(1, /0)" == pb"(0, 1)"
@test pn"(0, 1)" / pn"(/0, -1)" == pb"(-1, 0)"
@test pn"(0, 1)" / pn"(-1, 0)" == pb"(/0, 0)"
@test pn"(1, /0)" / pn"(1, /0)" == pb"(0, /0)"
@test pn"(1, /0)" / pn"(/0, -1)" == pb"(/0, 0)"
@test pn"(1, /0)" / pn"(-1, 0)" == pb"(/0, -1)"
@test pn"(/0, -1)" / pn"(/0, -1)" == pb"(0, /0)"
@test pn"(/0, -1)" / pn"(-1, 0)" == pb"(1, /0)"
@test pn"(-1, 0)" / pn"(-1, 0)" == pb"(0, /0)"

const allpbounds = let accum = Pbound[pb"empty"]
  for x in eachpnum(pb"everything"), y in eachpnum(pb"everything")
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

@test bisectvalue(x->x*(x-1)*(x+1), pn"0") == [ pb"-1", pb"0", pb"1" ]
@test bisectvalue(x->x*x + 1, pn"0") == Pbound[]
@test bisectvalue(x->x, pn"/0") == [ pb"/0" ]
@test bisectvalue(x->(1+x)/(1-x), pn"0") == [ pb"-1", pb"/0" ]
@test bisectvalue(x->(1+x)/(1-x), pn"/0") == [ pb"1", pb"/0" ]
@test bisectvalue(x->x*x, Pnum(2)) == [ pb"(/0, -1)", pb"(1, /0)" ]
@test bisectvalue(x->pb"(-1, 1)", pn"0") == [ pb"everything" ]

@test exp(pn"0") == pb"1"
@test exp(pn"(0, 1)") == pb"(1, /0)"
@test exp(pn"1") == pb"(1, /0)"
@test exp(pn"(1, /0)") == pb"(1, /0)"
@test exp(pn"/0") == pb"[0, /0]"
@test exp(pn"(/0, -1)") == pb"(0, 1)"
@test exp(pn"-1") == pb"(0, 1)"
@test exp(pn"(-1, 0)") == pb"(0, 1)"

for x1 in allpbounds
  @test Pnums.Sopn(exp(x1)) == exp(Pnums.Sopn(x1))
end
