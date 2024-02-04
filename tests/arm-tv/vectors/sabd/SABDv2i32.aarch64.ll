define <2 x i32> @f(<2 x i32> %0, <2 x i32> %1) {
  %3 = sub nsw <2 x i32> %0, %1
  %4 = sub nsw <2 x i32> %1, %0
  %5 = icmp sgt <2 x i32> %3, <i32 -1, i32 -1>
  %6 = select <2 x i1> %5, <2 x i32> %4, <2 x i32> %3
  %7 = icmp sgt <2 x i32> %6, <i32 -1, i32 -1>
  %8 = sub nsw <2 x i32> zeroinitializer, %6
  %9 = select <2 x i1> %7, <2 x i32> %6, <2 x i32> %8
  ret <2 x i32> %9
}