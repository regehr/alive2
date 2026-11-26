define <4 x i1> @test(<4 x float> %x, <4 x float> %y) {
  %r = fcmp olt <4 x float> %x, %y
  ret <4 x i1> %r
}
