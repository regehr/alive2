; CHECK: 1 incorrect transformations
; for (m1 a, m2 b, m1 c), c is in v9, not in v12 after b
define <vscale x 2 x i32> @test(<vscale x 2 x i32> %a, <vscale x 4 x i32> %b, <vscale x 2 x i32> %c) {
  %r = sub <vscale x 2 x i32> %c, %a
  ret <vscale x 2 x i32> %r
}
