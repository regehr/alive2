define <8 x i16> @f(<8 x i16> %0, <8 x i16> %1) {
  %3 = sext <8 x i16> %0 to <8 x i32>
  %4 = sext <8 x i16> %1 to <8 x i32>
  %5 = add <8 x i32> %3, %4
  %6 = add <8 x i32> %5, <i32 1, i32 1, i32 1, i32 1, i32 1, i32 1, i32 1, i32 1>
  %7 = ashr <8 x i32> %6, <i32 1, i32 1, i32 1, i32 1, i32 1, i32 1, i32 1, i32 1>
  %8 = trunc <8 x i32> %7 to <8 x i16>
  ret <8 x i16> %8
}