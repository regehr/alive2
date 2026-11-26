define <2 x double> @test(<2 x i32> %x) {
  %r = uitofp <2 x i32> %x to <2 x double>
  ret <2 x double> %r
}
