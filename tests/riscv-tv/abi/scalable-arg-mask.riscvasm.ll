; CHECK: ERROR: Unsupported function argument
; a mask argument goes in v0, which we don't model yet
define <vscale x 2 x i1> @test(<vscale x 2 x i1> %m) {
  ret <vscale x 2 x i1> %m
}
