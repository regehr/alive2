define void @f(ptr %0, ptr %1, ptr %2) {
  %4 = load <4 x i16>, ptr %0, align 8
  %5 = load <4 x i16>, ptr %1, align 8
  %6 = zext <4 x i16> %4 to <4 x i32>
  %7 = zext <4 x i16> %5 to <4 x i32>
  %8 = add <4 x i32> %6, %7
  store <4 x i32> %8, ptr %2, align 8
  ret void
}