; Function Attrs: nounwind
define void @f(ptr %0, ptr %1, ptr %2, ptr %3) {
  %5 = load <96 x i16>, ptr %0, align 64
  %6 = shufflevector <96 x i16> %5, <96 x i16> poison, <32 x i32> <i32 0, i32 3, i32 6, i32 9, i32 12, i32 15, i32 18, i32 21, i32 24, i32 27, i32 30, i32 33, i32 36, i32 39, i32 42, i32 45, i32 48, i32 51, i32 54, i32 57, i32 60, i32 63, i32 66, i32 69, i32 72, i32 75, i32 78, i32 81, i32 84, i32 87, i32 90, i32 93>
  store <32 x i16> %6, ptr %1, align 64
  ret void
}