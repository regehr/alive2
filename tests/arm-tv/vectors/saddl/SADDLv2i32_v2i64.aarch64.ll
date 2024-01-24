define <2 x i64> @f(ptr %0, ptr %1) {
  %3 = load <2 x i32>, ptr %0, align 8
  %4 = load <2 x i32>, ptr %1, align 8
  %5 = sext <2 x i32> %3 to <2 x i64>
  %6 = sext <2 x i32> %4 to <2 x i64>
  %7 = add <2 x i64> %5, %6
  ret <2 x i64> %7
}