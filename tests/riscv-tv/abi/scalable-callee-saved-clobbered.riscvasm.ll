; CHECK: 1 incorrect transformations
; a function that takes a vector argument follows the vector calling
; convention variant, which makes v24-v31 callee-saved
define <vscale x 2 x i32> @test(<vscale x 2 x i32> %a) {
  %r = add <vscale x 2 x i32> %a, %a
  ret <vscale x 2 x i32> %r
}
