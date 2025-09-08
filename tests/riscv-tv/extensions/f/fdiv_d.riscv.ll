define double @f(double %0, double %1) {
  %3 = fdiv double %1, %0
  ret double %3
}
