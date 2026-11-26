define <4 x float> @test(<4 x float> %x, float %s) {
  %h = insertelement <4 x float> poison, float %s, i64 0
  %r = shufflevector <4 x float> %h, <4 x float> %x, <4 x i32> <i32 0, i32 4, i32 5, i32 6>
  ret <4 x float> %r
}
