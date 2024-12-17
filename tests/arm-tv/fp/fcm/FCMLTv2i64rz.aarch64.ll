define <2 x double> @f(<2 x double> %0) {
  %2 = fcmp uge <2 x double> %0, zeroinitializer
  %3 = fneg nsz <2 x double> %0
  %4 = select <2 x i1> %2, <2 x double> %0, <2 x double> %3
  ret <2 x double> %4
}
