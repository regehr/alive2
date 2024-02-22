define <4 x i32> @load_splat_v4i32_a2(ptr %0) {
  %2 = load i32, ptr %0, align 2
  %3 = insertelement <4 x i32> undef, i32 %2, i32 0
  %4 = shufflevector <4 x i32> %3, <4 x i32> undef, <4 x i32> zeroinitializer
  ret <4 x i32> %4
}