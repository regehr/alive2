define <2 x float> @f(<2 x float> %0) {
  %2 = fadd <2 x float> %0, <float 7.0, float 8.0>
  ret <2 x float> %2
}
