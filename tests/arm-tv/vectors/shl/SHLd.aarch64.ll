; Function Attrs: nounwind
define <1 x i64> @f(ptr %0) {
  %2 = load <1 x i64>, ptr %0, align 8
  %3 = shl <1 x i64> %2, <i64 63>
  ret <1 x i64> %3
}