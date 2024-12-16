define <4 x float> @f(<4 x float> %0, <4 x float> %1) {
  %3 = fsub <4 x float> %1, %0
  %4 = select <4 x i1> <i1 false, i1 true, i1 true, i1 true>, <4 x float> %1, <4 x float> %3
  ret <4 x float> %4
}
