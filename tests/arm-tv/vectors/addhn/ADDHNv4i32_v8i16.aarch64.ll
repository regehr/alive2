; Function Attrs: nounwind
define <8 x i16> @f(<4 x i16> %0, ptr %1, ptr %2) {
  %4 = load <4 x i32>, ptr %1, align 16
  %5 = load <4 x i32>, ptr %2, align 16
  %6 = add <4 x i32> %4, %5
  %7 = lshr <4 x i32> %6, <i32 16, i32 16, i32 16, i32 16>
  %8 = trunc <4 x i32> %7 to <4 x i16>
  %9 = shufflevector <4 x i16> %0, <4 x i16> %8, <8 x i32> <i32 0, i32 1, i32 2, i32 3, i32 4, i32 5, i32 6, i32 7>
  ret <8 x i16> %9
}