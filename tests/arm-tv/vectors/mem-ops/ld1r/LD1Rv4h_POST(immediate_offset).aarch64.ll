define <8 x i16> @f(<8 x i16> %0, ptr nocapture readonly %1) {
  %3 = load i16, ptr %1, align 2
  %4 = insertelement <4 x i16> undef, i16 %3, i32 0
  %5 = shufflevector <4 x i16> %4, <4 x i16> undef, <4 x i32> zeroinitializer
  %6 = getelementptr inbounds i16, ptr %1, i32 1
  %7 = load i16, ptr %6, align 2
  %8 = insertelement <4 x i16> undef, i16 %7, i32 0
  %9 = shufflevector <4 x i16> %8, <4 x i16> undef, <4 x i32> zeroinitializer
  %10 = shufflevector <4 x i16> %5, <4 x i16> %9, <8 x i32> <i32 0, i32 1, i32 2, i32 3, i32 4, i32 5, i32 6, i32 7>
  ret <8 x i16> %10
}