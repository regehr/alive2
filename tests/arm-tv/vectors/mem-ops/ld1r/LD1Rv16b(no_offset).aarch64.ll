define <16 x i8> @vecucuc(ptr nocapture readonly %0) {
  %2 = load i8, ptr %0, align 1
  %3 = insertelement <16 x i8> undef, i8 %2, i32 0
  %4 = shufflevector <16 x i8> %3, <16 x i8> undef, <16 x i32> zeroinitializer
  ret <16 x i8> %4
}