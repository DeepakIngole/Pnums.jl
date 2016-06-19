module Pnums

include("types.jl")
include("ops.jl")
include("io.jl")
include("print_decimal.jl")

export
  Pbound,
  Pnum3,
  Pbound3,
  @pn3_str,
  @pb3_str,
  Pnum4,
  Pbound4,
  @pn4_str,
  @pb4_str,
  Pnum8,
  Pbound8,
  @pn8_str,
  @pb8_str,
  Pnum16,
  Pbound16,
  @pn16_str,
  @pb16_str,
  bisectvalue,
  bisectroot,
  bisectmaximum,
  nextpnum,
  prevpnum,
  eachpnum

end
