; two m1 vector arguments arrive in v8 and v9, and the result goes back in v8
define <vscale x 2 x i32> @test(<vscale x 2 x i32> %a, <vscale x 2 x i32> %b) {
  %r = add <vscale x 2 x i32> %a, %b
  ret <vscale x 2 x i32> %r
}
