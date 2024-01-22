; Function Attrs: nounwind
define <2 x i64> @f(<2 x i64> %0, <2 x i32> %1, <2 x i32> %2) {
  %4 = zext <2 x i32> %1 to <2 x i64>
  %5 = zext <2 x i32> %2 to <2 x i64>
  %6 = mul <2 x i64> %4, %5
  %7 = add <2 x i64> %0, %6
  %8 = and <2 x i64> %7, <i64 4294967295, i64 4294967295>
  ret <2 x i64> %8
}