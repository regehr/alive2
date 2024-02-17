; Function Attrs: nounwind
define <4 x i32> @f(<4 x i32> %0, <4 x i32> %1) {
  %3 = icmp ult <4 x i32> %0, %1
  %4 = sub <4 x i32> %0, %1
  %5 = sub <4 x i32> %1, %0
  %6 = select <4 x i1> %3, <4 x i32> %5, <4 x i32> %4
  ret <4 x i32> %6
}