; the first mask argument arrives in v0, the second in v8, and a mask
; result goes back in v0
define <vscale x 4 x i1> @test(<vscale x 4 x i1> %a, <vscale x 4 x i1> %b) {
  %r = and <vscale x 4 x i1> %a, %b
  ret <vscale x 4 x i1> %r
}
