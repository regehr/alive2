define double @test(<2 x double> %x) {
  %r = extractelement <2 x double> %x, i64 1
  ret double %r
}
