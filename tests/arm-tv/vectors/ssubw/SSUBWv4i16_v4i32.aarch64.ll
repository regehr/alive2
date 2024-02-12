; Function Attrs: nounwind
define <4 x i32> @f(ptr %0, ptr %1) {
  %3 = load <4 x i32>, ptr %0, align 16
  %4 = load <4 x i16>, ptr %1, align 8
  %5 = sext <4 x i16> %4 to <4 x i32>
  %6 = sub <4 x i32> %3, %5
  ret <4 x i32> %6
}