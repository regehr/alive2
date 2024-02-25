; Function Attrs: nounwind
define <2 x i64> @f(ptr %0) {
  %2 = load <16 x i64>, ptr %0, align 128
  %3 = shufflevector <16 x i64> %2, <16 x i64> undef, <2 x i32> <i32 2, i32 6>
  ret <2 x i64> %3
}