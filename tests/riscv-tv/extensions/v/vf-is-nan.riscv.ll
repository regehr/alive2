declare <2 x i1> @llvm.is.fpclass.v2f32(<2 x float>, i32)
define <2 x i1> @test(<2 x float> %x) {
  %r = call <2 x i1> @llvm.is.fpclass.v2f32(<2 x float> %x, i32 516)
  ret <2 x i1> %r
}
