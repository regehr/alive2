define <2 x i64> @load_splat_v2i64_a4(ptr %0) {
  %2 = load i64, ptr %0, align 4
  %3 = insertelement <2 x i64> undef, i64 %2, i32 0
  %4 = shufflevector <2 x i64> %3, <2 x i64> undef, <2 x i32> zeroinitializer
  ret <2 x i64> %4
}