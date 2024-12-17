define <2 x double> @f(<2 x double> %0, <2 x double> %1, <2 x double> %2) {
  %4 = shufflevector <2 x double> %0, <2 x double> undef, <2 x i32> <i32 1, i32 0>
  %5 = fcmp oeq <2 x double> %2, zeroinitializer
  %6 = select <2 x i1> %5, <2 x double> %4, <2 x double> %1
  ret <2 x double> %6
}
