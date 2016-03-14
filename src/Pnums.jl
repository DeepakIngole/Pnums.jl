module Pnums

include("types.jl")
include("ops.jl")
include("io.jl")

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
  bisect,
  bisectvalue,
  eachpnum

end
