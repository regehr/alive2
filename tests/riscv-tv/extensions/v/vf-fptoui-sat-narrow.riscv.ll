declare <2 x i32> @llvm.fptoui.sat.v2i32.v2f64(<2 x double>)
define <2 x i32> @test(<2 x double> %x) {
  %r = call <2 x i32> @llvm.fptoui.sat.v2i32.v2f64(<2 x double> %x)
  ret <2 x i32> %r
}
