define <2 x double> @f(double %0) {
  %2 = insertelement <2 x double> poison, double %0, i32 0
  %3 = fsub <2 x double> %2, <double 4.200000e+01, double -4.200000e+01>
  ret <2 x double> %3
}
