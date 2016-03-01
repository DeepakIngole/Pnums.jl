using Pnums
using Base.Test

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
@test recip(pn"0") == pn"/0"
@test recip(pn"(0, 1)") == pn"(1, /0)"
@test recip(pn"1") == pn"1"
@test recip(pn"(1, /0)") == pn"(0, 1)"
@test recip(pn"/0") == pn"0"
@test recip(pn"(/0, -1)") == pn"(-1, 0)"
@test recip(pn"-1") == pn"-1"
@test recip(pn"(-1, 0)") == pn"(/0, -1)"

# isexact
@test isexact(pn"0") == true
@test isexact(pn"(0, 1)") == false
@test isexact(pn"1") == true
@test isexact(pn"(1, /0)") == false
@test isexact(pn"/0") == true
@test isexact(pn"(/0, -1)") == false
@test isexact(pn"-1") == true
@test isexact(pn"(-1, 0)") == false

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
