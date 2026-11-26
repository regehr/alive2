define <2 x float> @test(<2 x float> %x, <2 x float> %y) {
  %r = fmul <2 x float> %x, %y
  ret <2 x float> %r
}
