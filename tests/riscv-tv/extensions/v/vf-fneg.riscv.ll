define <2 x float> @test(<2 x float> %x) {
  %r = fneg <2 x float> %x
  ret <2 x float> %r
}
