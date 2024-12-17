define i1 @f(float %0) {
  %2 = fcmp olt float %0, 0.000000e+00
  %3 = fcmp ogt float %0, -0.000000e+00
  %4 = and i1 %2, %3
  ret i1 %4
}
