define <2 x i32> @f(<2 x i32> %0) {
  %2 = ashr <2 x i32> %0, <i32 31, i32 31>
  %3 = and <2 x i32> %2, <i32 8, i32 8>
  %4 = add <2 x i32> %3, %2
  ret <2 x i32> %4
}