define <1 x double> @test(<1 x float> %x, <1 x float> %y) {
  %a = fpext <1 x float> %x to <1 x double>
  %b = fpext <1 x float> %y to <1 x double>
  %r = fmul <1 x double> %a, %b
  ret <1 x double> %r
}
