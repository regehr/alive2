define <2 x i32> @test(<2 x float> %x) {
  %r = call <2 x i32> @llvm.fptoui.sat.v2i32.v2f32(<2 x float> %x)
  ret <2 x i32> %r
}
