define noundef range(i8 -1, 2) i8 @f4(i16 noundef zeroext %0, i16 noundef zeroext %1) {
  %3 = icmp ult i16 %0, %1
  %4 = select i1 %3, i8 -1, i8 1
  %5 = icmp eq i16 %0, %1
  %6 = select i1 %5, i8 0, i8 %4
  ret i8 %6
}
