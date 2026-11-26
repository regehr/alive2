; the psABI example: for (m1 a, m2 b, m1 c), the search for each argument
; starts again at v8, so a gets v8, b gets v10-v11, and c gets v9
define <vscale x 2 x i32> @test(<vscale x 2 x i32> %a, <vscale x 4 x i32> %b, <vscale x 2 x i32> %c) {
  %r = sub <vscale x 2 x i32> %c, %a
  ret <vscale x 2 x i32> %r
}
