define <8 x i8> @dup_ld1_from_stack(ptr %0) {
  %2 = alloca i8, align 1
  %3 = load i8, ptr %2, align 1
  %4 = insertelement <8 x i8> poison, i8 %3, i32 0
  %5 = shufflevector <8 x i8> %4, <8 x i8> %4, <8 x i32> zeroinitializer
  ret <8 x i8> %5
}