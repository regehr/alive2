define <2 x double> @test(<2 x float> %x) {
  %r = fpext <2 x float> %x to <2 x double>
  ret <2 x double> %r
}
