define float @f(<4 x float> %0) {
  %2 = fadd <4 x float> %0, <float 1.000000e+00, float 2.000000e+00, float 3.000000e+00, float 4.200000e+01>
  %3 = extractelement <4 x float> %2, i32 2
  ret float %3
}
