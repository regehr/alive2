define <2 x float> @test(<2 x float> %x, float %f) {
  %r = insertelement <2 x float> %x, float %f, i64 1
  ret <2 x float> %r
}
