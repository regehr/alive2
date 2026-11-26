define <2 x double> @test(<2 x double> %x, double %s) {
  %i = insertelement <2 x double> poison, double %s, i64 0
  %sp = shufflevector <2 x double> %i, <2 x double> poison, <2 x i32> zeroinitializer
  %r = fsub <2 x double> %sp, %x
  ret <2 x double> %r
}
