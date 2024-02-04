define <8 x i16> @f(<8 x i16> %0, <8 x i16> %1) local_unnamed_addr {
  %3 = sub nsw <8 x i16> %0, %1
  %4 = icmp sgt <8 x i16> %3, <i16 -1, i16 -1, i16 -1, i16 -1, i16 -1, i16 -1, i16 -1, i16 -1>
  %5 = sub <8 x i16> zeroinitializer, %3
  %6 = select <8 x i1> %4, <8 x i16> %3, <8 x i16> %5
  ret <8 x i16> %6
}