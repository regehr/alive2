define <1 x float> @test(<1 x float> %x) {
  %r = call <1 x float> @llvm.sqrt.v1f32(<1 x float> %x)
  ret <1 x float> %r
}
