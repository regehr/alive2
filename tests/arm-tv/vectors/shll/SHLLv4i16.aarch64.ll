; Function Attrs: nounwind
define <4 x i32> @f(ptr %0) {
  %2 = load <4 x i16>, ptr %0, align 8
  %3 = zext <4 x i16> %2 to <4 x i32>
  %4 = shl <4 x i32> %3, <i32 16, i32 16, i32 16, i32 16>
  ret <4 x i32> %4
}