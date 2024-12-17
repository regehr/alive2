define i1 @f(float %0, float %1) {
  %3 = fdiv nnan float %0, %1
  %4 = fcmp ord float %0, %1
  %5 = fcmp ord float %3, %0
  %6 = and i1 %4, %5
  ret i1 %6
}
