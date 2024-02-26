; Function Attrs: nounwind
define <4 x i32> @f(<2 x i32> %0, ptr %1, ptr %2) {
  %4 = load <2 x i64>, ptr %1, align 16
  %5 = load <2 x i64>, ptr %2, align 16
  %6 = sub <2 x i64> %4, %5
  %7 = lshr <2 x i64> %6, <i64 32, i64 32>
  %8 = trunc <2 x i64> %7 to <2 x i32>
  %9 = shufflevector <2 x i32> %0, <2 x i32> %8, <4 x i32> <i32 0, i32 1, i32 2, i32 3>
  ret <4 x i32> %9
}