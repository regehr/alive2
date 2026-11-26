define <2 x float> @test(<2 x half> %x) {
  %r = fpext <2 x half> %x to <2 x float>
  ret <2 x float> %r
}
