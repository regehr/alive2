define <2 x double> @test(double %f) {
  %i = insertelement <2 x double> poison, double %f, i64 0
  %r = shufflevector <2 x double> %i, <2 x double> poison, <2 x i32> zeroinitializer
  ret <2 x double> %r
}
