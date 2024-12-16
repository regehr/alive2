define <2 x float> @f(<2 x float> %0) {
  %2 = shufflevector <2 x float> %0, <2 x float> poison, <2 x i32> zeroinitializer
  %3 = fsub <2 x float> splat (float 4.200000e+01), %2
  ret <2 x float> %3
}
