define void @f(ptr %0, ptr %1) {
  %3 = load <12 x i16>, ptr %0, align 4
  %4 = shufflevector <12 x i16> %3, <12 x i16> undef, <4 x i32> <i32 0, i32 3, i32 6, i32 9>
  %5 = shufflevector <12 x i16> %3, <12 x i16> undef, <4 x i32> <i32 1, i32 4, i32 7, i32 10>
  %6 = shufflevector <12 x i16> %3, <12 x i16> undef, <4 x i32> <i32 2, i32 5, i32 8, i32 11>
  %7 = add <4 x i16> %4, %5
  %8 = add <4 x i16> %7, %6
  store <4 x i16> %8, ptr %1, align 8
  ret void
}