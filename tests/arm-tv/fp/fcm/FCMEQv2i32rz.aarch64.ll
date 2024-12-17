define <2 x float> @f(<2 x float> %0) {
  %2 = fcmp oeq <2 x float> %0, zeroinitializer
  %3 = fmul <2 x float> %0, <float 3.200000e+01, float 6.400000e+01>
  %4 = select <2 x i1> %2, <2 x float> %3, <2 x float> %0
  ret <2 x float> %4
}
