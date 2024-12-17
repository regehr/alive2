define <2 x double> @f(<2 x double> %0, <2 x double> %1, i64 %2) {
  %4 = bitcast <2 x double> %0 to <2 x i64>
  %5 = insertelement <2 x i64> undef, i64 %2, i32 0
  %6 = shufflevector <2 x i64> %5, <2 x i64> undef, <2 x i32> zeroinitializer
  %7 = and <2 x i64> %4, %6
  %8 = bitcast <2 x i64> %7 to <2 x double>
  %9 = fcmp oge <2 x double> %8, %1
  %10 = select <2 x i1> %9, <2 x double> %8, <2 x double> %1
  ret <2 x double> %10
}
