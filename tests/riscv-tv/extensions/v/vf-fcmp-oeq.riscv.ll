define <2 x i1> @test(<2 x double> %x, <2 x double> %y) {
  %r = fcmp oeq <2 x double> %x, %y
  ret <2 x i1> %r
}
