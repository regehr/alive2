define <2 x double> @f(<2 x double> %0) {
  %2 = fsub nnan <2 x double> <double 0xFFF0000000000000, double poison>, %0
  ret <2 x double> %2
}
