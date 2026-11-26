define <4 x float> @test(<4 x float> %x, <4 x float> %y) {
  %n = fneg <4 x float> %y
  %r = call <4 x float> @llvm.copysign.v4f32(<4 x float> %x, <4 x float> %n)
  ret <4 x float> %r
}
