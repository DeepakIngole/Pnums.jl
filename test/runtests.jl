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
