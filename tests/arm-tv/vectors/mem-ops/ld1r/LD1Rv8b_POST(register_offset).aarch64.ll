; Function Attrs: nounwind
define <8 x i8> @vld1dupi8_postinc_register(ptr nocapture %0, i32 %1) {
  %3 = load ptr, ptr %0, align 4
  %4 = load i8, ptr %3, align 1
  %5 = insertelement <8 x i8> undef, i8 %4, i32 0
  %6 = shufflevector <8 x i8> %5, <8 x i8> undef, <8 x i32> zeroinitializer
  %7 = getelementptr inbounds i8, ptr %3, i32 %1
  store ptr %7, ptr %0, align 4
  ret <8 x i8> %6
}