declare <2 x double> @llvm.fabs.v2f64(<2 x double>)
define <2 x double> @test(<2 x double> %x) {
  %r = call <2 x double> @llvm.fabs.v2f64(<2 x double> %x)
  ret <2 x double> %r
}
