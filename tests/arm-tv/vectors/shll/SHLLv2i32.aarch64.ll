; Function Attrs: nounwind
define <2 x i64> @f(ptr %0) {
  %2 = load <2 x i32>, ptr %0, align 8
  %3 = zext <2 x i32> %2 to <2 x i64>
  %4 = shl <2 x i64> %3, <i64 32, i64 32>
  ret <2 x i64> %4
}