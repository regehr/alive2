; Function Attrs: nounwind
define <2 x i32> @f(ptr %0, ptr %1) {
  %3 = load <2 x i64>, ptr %0, align 16
  %4 = load <2 x i64>, ptr %1, align 16
  %5 = add <2 x i64> %3, %4
  %6 = lshr <2 x i64> %5, <i64 32, i64 32>
  %7 = trunc <2 x i64> %6 to <2 x i32>
  ret <2 x i32> %7
}