declare <2 x double> @llvm.minimumnum.v2f64(<2 x double>, <2 x double>)
define <2 x double> @test(<2 x double> %x, <2 x double> %y) {
  %r = call <2 x double> @llvm.minimumnum.v2f64(<2 x double> %x, <2 x double> %y)
  ret <2 x double> %r
}
