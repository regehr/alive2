define <2 x float> @test(<2 x float> %x, float %s) {
  %i = insertelement <2 x float> poison, float %s, i64 0
  %sp = shufflevector <2 x float> %i, <2 x float> poison, <2 x i32> zeroinitializer
  %r = fdiv <2 x float> %sp, %x
  ret <2 x float> %r
}
