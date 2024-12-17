define <2 x float> @f(<2 x float> %0) {
  %2 = fcmp olt <2 x float> %0, zeroinitializer
  %3 = select <2 x i1> %2, <2 x float> zeroinitializer, <2 x float> %0
  ret <2 x float> %3
}
