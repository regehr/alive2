define <2 x i1> @test(<2 x float> %x, float %s) {
  %i = insertelement <2 x float> poison, float %s, i64 0
  %sp = shufflevector <2 x float> %i, <2 x float> poison, <2 x i32> zeroinitializer
  %c = fcmp ogt <2 x float> %x, %sp
  ret <2 x i1> %c
}
