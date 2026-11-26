define <2 x half> @test(<2 x float> %x) {
  %r = fptrunc <2 x float> %x to <2 x half>
  ret <2 x half> %r
}
