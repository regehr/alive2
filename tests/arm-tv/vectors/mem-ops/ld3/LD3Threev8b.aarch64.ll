define <8 x i8> @f(ptr %0) {
  %2 = load <24 x i8>, ptr %0, align 1
  %3 = shufflevector <24 x i8> %2, <24 x i8> undef, <8 x i32> <i32 2, i32 5, i32 8, i32 11, i32 14, i32 17, i32 20, i32 23>
  ret <8 x i8> %3
}