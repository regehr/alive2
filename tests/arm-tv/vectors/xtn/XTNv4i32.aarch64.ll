; Function Attrs: nounwind
define <4 x i32> @xtn2_4s(<2 x i32> %0, <2 x i64> %1) {
  %3 = trunc <2 x i64> %1 to <2 x i32>
  %4 = shufflevector <2 x i32> %0, <2 x i32> %3, <4 x i32> <i32 0, i32 1, i32 2, i32 3>
  ret <4 x i32> %4
}