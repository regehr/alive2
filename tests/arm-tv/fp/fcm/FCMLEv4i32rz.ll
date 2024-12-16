define <4 x float> @f(<4 x float> %0) {
  %2 = fcmp ole <4 x float> %0, zeroinitializer
  %3 = fneg <4 x float> %0
  %4 = select <4 x i1> %2, <4 x float> %3, <4 x float> %0
  ret <4 x float> %4
}
