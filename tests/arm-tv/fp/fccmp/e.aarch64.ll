define i1 @f(float %0, float %1) {
  %3 = fcmp ord float %0, 0.000000e+00
  %4 = call float @llvm.fabs.f32(float %0)
  %5 = fcmp olt float %4, %1
  %6 = and i1 %3, %5
  ret i1 %6
}
