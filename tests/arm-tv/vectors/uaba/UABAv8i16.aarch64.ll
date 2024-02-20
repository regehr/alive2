; Function Attrs: nounwind
define <8 x i16> @f(<8 x i16> %0, <8 x i16> %1) {
  %3 = icmp uge <8 x i16> %0, %1
  %4 = sub <8 x i16> %0, %1
  %5 = sub <8 x i16> %1, %0
  %6 = select <8 x i1> %3, <8 x i16> %4, <8 x i16> %5
  %7 = add <8 x i16> %4, %6
  ret <8 x i16> %7
}