; CHECK: 1 incorrect transformations
; vror.vv rotates right, not left
define <4 x i32> @test(<4 x i32> %x, <4 x i32> %y) {
  %r = call <4 x i32> @llvm.fshl.v4i32(<4 x i32> %x, <4 x i32> %x, <4 x i32> %y)
  ret <4 x i32> %r
}
