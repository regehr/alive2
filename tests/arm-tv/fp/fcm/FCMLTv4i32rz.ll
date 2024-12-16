define <4 x float> @f(<4 x float> %0, <4 x float> %1) {
  %3 = fcmp olt <4 x float> %0, zeroinitializer
  %4 = select <4 x i1> %3, <4 x float> %0, <4 x float> %1
  ret <4 x float> %4
}
