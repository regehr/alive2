define <2 x double> @f(<2 x double> %0) {
  %2 = fneg <2 x double> %0
  %3 = fcmp oge <2 x double> %0, zeroinitializer
  %4 = select <2 x i1> %3, <2 x double> %2, <2 x double> %0
  ret <2 x double> %4
}
