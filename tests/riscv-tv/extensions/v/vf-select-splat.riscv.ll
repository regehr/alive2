define <2 x float> @test(<2 x i1> %c, <2 x float> %x, float %f) {
  %i = insertelement <2 x float> poison, float %f, i64 0
  %sp = shufflevector <2 x float> %i, <2 x float> poison, <2 x i32> zeroinitializer
  %r = select <2 x i1> %c, <2 x float> %sp, <2 x float> %x
  ret <2 x float> %r
}
