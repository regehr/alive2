define <4 x i32> @f(<4 x i32> %0) {
  %2 = xor <4 x i32> %0, <i32 -1, i32 -1, i32 -1, i32 -1>
  %3 = lshr <4 x i32> %2, <i32 31, i32 31, i32 31, i32 31>
  %4 = add <4 x i32> %3, <i32 42, i32 42, i32 42, i32 42>
  ret <4 x i32> %4
}