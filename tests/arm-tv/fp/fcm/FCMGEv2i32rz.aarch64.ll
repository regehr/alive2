define <2 x float> @f(<2 x float> %0) {
  %2 = fcmp ult <2 x float> %0, zeroinitializer
  %3 = fadd <2 x float> %0, splat (float 1.000000e+00)
  %4 = select nnan nsz <2 x i1> %2, <2 x float> %3, <2 x float> splat (float 1.000000e+00)
  ret <2 x float> %4
}
