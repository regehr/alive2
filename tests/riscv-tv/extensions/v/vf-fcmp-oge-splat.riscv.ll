define <4 x i1> @test(<4 x float> %x, float %s) {
  %h = insertelement <4 x float> poison, float %s, i64 0
  %sp = shufflevector <4 x float> %h, <4 x float> poison, <4 x i32> zeroinitializer
  %r = fcmp oge <4 x float> %x, %sp
  ret <4 x i1> %r
}
