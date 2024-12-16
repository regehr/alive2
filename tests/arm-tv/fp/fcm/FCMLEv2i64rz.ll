define <2 x i64> @f(<2 x double> %0) {
  %2 = fcmp ole <2 x double> %0, zeroinitializer
  %3 = sext <2 x i1> %2 to <2 x i64>
  ret <2 x i64> %3
}
