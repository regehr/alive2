define <2 x double> @test(<2 x double> %x, <2 x double> %y) {
  %c = fcmp ole <2 x double> %x, %y
  %r = select <2 x i1> %c, <2 x double> %x, <2 x double> %y
  ret <2 x double> %r
}
