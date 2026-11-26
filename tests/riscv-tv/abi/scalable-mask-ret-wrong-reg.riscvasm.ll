; CHECK: 1 incorrect transformations
; a mask result has to be returned in v0, not v8
define <vscale x 4 x i1> @test(<vscale x 4 x i1> %a, <vscale x 4 x i1> %b) {
  %r = and <vscale x 4 x i1> %a, %b
  ret <vscale x 4 x i1> %r
}
