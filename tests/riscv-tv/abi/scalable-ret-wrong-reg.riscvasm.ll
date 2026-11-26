; CHECK: 1 incorrect transformations
; a vector result has to be returned in v8, not v9
define <vscale x 2 x i32> @test(<vscale x 2 x i32> %a, <vscale x 2 x i32> %b) {
  %r = add <vscale x 2 x i32> %a, %b
  ret <vscale x 2 x i32> %r
}
