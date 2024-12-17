define <2 x i64> @f(<2 x double> %0, <2 x double> %1) {
  %3 = fcmp nnan one <2 x double> %0, %1
  %4 = sext <2 x i1> %3 to <2 x i64>
  ret <2 x i64> %4
}
