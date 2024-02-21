; Function Attrs: nounwind
define <2 x i64> @f(ptr %0, ptr %1) {
  %3 = load <2 x i64>, ptr %0, align 16
  %4 = ashr <2 x i64> %3, <i64 1, i64 1>
  %5 = load <2 x i64>, ptr %1, align 16
  %6 = add <2 x i64> %4, %5
  ret <2 x i64> %6
}