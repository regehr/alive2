; CHECK: 1 incorrect transformations
; the second mask is in v9, not v8, which holds d
define <vscale x 2 x i1> @test(<vscale x 2 x i32> %d, <vscale x 2 x i1> %m1, <vscale x 2 x i1> %m2) {
  %r = and <vscale x 2 x i1> %m1, %m2
  ret <vscale x 2 x i1> %r
}
