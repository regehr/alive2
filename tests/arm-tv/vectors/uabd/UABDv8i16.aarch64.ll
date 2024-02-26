; Function Attrs: nounwind
define <8 x i16> @f(<8 x i16> %0, <8 x i16> %1) {
  %3 = icmp ugt <8 x i16> %0, %1
  %4 = select <8 x i1> %3, <8 x i16> <i16 -1, i16 -1, i16 -1, i16 -1, i16 -1, i16 -1, i16 -1, i16 -1>, <8 x i16> <i16 1, i16 1, i16 1, i16 1, i16 1, i16 1, i16 1, i16 1>
  %5 = select <8 x i1> %3, <8 x i16> %1, <8 x i16> %0
  %6 = select <8 x i1> %3, <8 x i16> %0, <8 x i16> %1
  %7 = sub <8 x i16> %6, %5
  %8 = lshr <8 x i16> %7, <i16 1, i16 1, i16 1, i16 1, i16 1, i16 1, i16 1, i16 1>
  %9 = mul <8 x i16> %8, %4
  %10 = add <8 x i16> %9, %0
  ret <8 x i16> %10
}