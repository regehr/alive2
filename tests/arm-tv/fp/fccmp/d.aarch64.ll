define zeroext i1 @f(float %0, float %1) {
  %3 = fcmp oge float %0, %1
  %4 = fcmp ole float %1, 0.000000e+00
  %5 = and i1 %3, %4
  ret i1 %5
}
