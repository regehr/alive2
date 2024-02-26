define <2 x i64> @f(<2 x i32> %0, <2 x i32> %1) {
  %3 = sext <2 x i32> %0 to <2 x i64>
  %4 = sext <2 x i32> %1 to <2 x i64>
  %5 = sub nsw <2 x i64> %3, %4
  %6 = icmp slt <2 x i64> %5, zeroinitializer
  %7 = sub nsw <2 x i64> zeroinitializer, %5
  %8 = select <2 x i1> %6, <2 x i64> %7, <2 x i64> %5
  ret <2 x i64> %8
}