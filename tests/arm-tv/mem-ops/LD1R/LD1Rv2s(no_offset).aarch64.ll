define <2 x i32> @ld1r_2s_int_shuff(ptr nocapture %0) {
  %2 = load i32, ptr %0, align 4
  %3 = insertelement <2 x i32> undef, i32 %2, i32 0
  %4 = shufflevector <2 x i32> %3, <2 x i32> undef, <2 x i32> zeroinitializer
  ret <2 x i32> %4
}