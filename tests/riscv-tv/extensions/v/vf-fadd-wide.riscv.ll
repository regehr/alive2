define <2 x double> @test(<2 x float> %x, <2 x float> %y) {
  %a = fpext <2 x float> %x to <2 x double>
  %b = fpext <2 x float> %y to <2 x double>
  %r = fadd <2 x double> %a, %b
  ret <2 x double> %r
}
