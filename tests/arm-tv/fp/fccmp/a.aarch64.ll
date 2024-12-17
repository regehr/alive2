define i1 @f(float %0, float %1) {
  %3 = fcmp ord float %0, 0.000000e+00
  %4 = fcmp olt float %1, %0
  %5 = and i1 %3, %4
  ret i1 %5
}
