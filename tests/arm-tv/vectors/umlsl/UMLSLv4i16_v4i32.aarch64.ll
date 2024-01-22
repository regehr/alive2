; Function Attrs: nounwind
define <4 x i32> @f(<4 x i32> %0, <4 x i16> %1, <4 x i16> %2) {
  %4 = zext <4 x i16> %1 to <4 x i32>
  %5 = zext <4 x i16> %2 to <4 x i32>
  %6 = mul <4 x i32> %4, %5
  %7 = sub <4 x i32> %0, %6
  ret <4 x i32> %7
}