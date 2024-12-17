define <4 x float> @f(<4 x float> %0, ptr %1, <4 x float> %2, <4 x float> %3) {
  %5 = load <4 x float>, ptr %1, align 16
  %6 = shufflevector <4 x float> %0, <4 x float> %5, <4 x i32> <i32 3, i32 3, i32 5, i32 7>
  %7 = fcmp oeq <4 x float> %3, zeroinitializer
  %8 = select <4 x i1> %7, <4 x float> %6, <4 x float> %2
  ret <4 x float> %8
}
