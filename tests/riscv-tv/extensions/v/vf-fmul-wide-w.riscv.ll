define <2 x double> @test(<2 x double> %x, <2 x float> %y) {
  %b = fpext <2 x float> %y to <2 x double>
  %r = fsub <2 x double> %x, %b
  ret <2 x double> %r
}
