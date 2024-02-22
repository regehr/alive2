; Function Attrs: nounwind
define <16 x i8> @vld1dupqi8_postinc_fixed(ptr nocapture %0) {
  %2 = load ptr, ptr %0, align 4
  %3 = load i8, ptr %2, align 1
  %4 = insertelement <16 x i8> undef, i8 %3, i32 0
  %5 = shufflevector <16 x i8> %4, <16 x i8> undef, <16 x i32> zeroinitializer
  %6 = getelementptr inbounds i8, ptr %2, i32 1
  store ptr %6, ptr %0, align 4
  ret <16 x i8> %5
}