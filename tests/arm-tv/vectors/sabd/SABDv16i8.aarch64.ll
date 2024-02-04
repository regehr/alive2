define <16 x i8> @f(<16 x i8> %0, <16 x i8> %1) local_unnamed_addr {
  %3 = sub nsw <16 x i8> %0, %1
  %4 = icmp sgt <16 x i8> %3, <i8 -1, i8 -1, i8 -1, i8 -1, i8 -1, i8 -1, i8 -1, i8 -1, i8 -1, i8 -1, i8 -1, i8 -1, i8 -1, i8 -1, i8 -1, i8 -1>
  %5 = sub <16 x i8> zeroinitializer, %3
  %6 = select <16 x i1> %4, <16 x i8> %3, <16 x i8> %5
  ret <16 x i8> %6
}