; Function Attrs: nounwind
define <1 x i64> @f(ptr %0, ptr %1) {
  %3 = load <1 x i64>, ptr %0, align 8
  %4 = load <1 x i64>, ptr %1, align 8
  %5 = ashr <1 x i64> %4, <i64 63>
  %6 = add <1 x i64> %3, %5
  ret <1 x i64> %6
}