define <8 x i16> @f(<8 x i16> %0) {
  %2 = sext <8 x i16> %0 to <8 x i32>
  %3 = add <8 x i32> <i32 1, i32 1, i32 1, i32 1, i32 1, i32 1, i32 1, i32 1>, %2
  %4 = ashr <8 x i32> %3, <i32 1, i32 1, i32 1, i32 1, i32 1, i32 1, i32 1, i32 1>
  %5 = trunc <8 x i32> %4 to <8 x i16>
  ret <8 x i16> %5
}