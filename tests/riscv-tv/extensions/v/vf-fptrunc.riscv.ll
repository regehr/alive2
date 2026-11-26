define <2 x float> @test(<2 x double> %x) {
  %r = fptrunc <2 x double> %x to <2 x float>
  ret <2 x float> %r
}
