define void @f(ptr %0, ptr %1) {
  %3 = load <24 x i16>, ptr %0, align 4
  %4 = shufflevector <24 x i16> %3, <24 x i16> undef, <8 x i32> <i32 0, i32 3, i32 6, i32 9, i32 12, i32 15, i32 18, i32 21>
  %5 = shufflevector <24 x i16> %3, <24 x i16> undef, <8 x i32> <i32 1, i32 4, i32 7, i32 10, i32 13, i32 16, i32 19, i32 22>
  %6 = shufflevector <24 x i16> %3, <24 x i16> undef, <8 x i32> <i32 2, i32 5, i32 8, i32 11, i32 14, i32 17, i32 20, i32 23>
  %7 = add <8 x i16> %4, %5
  %8 = add <8 x i16> %7, %6
  store <8 x i16> %8, ptr %1, align 16
  ret void
}