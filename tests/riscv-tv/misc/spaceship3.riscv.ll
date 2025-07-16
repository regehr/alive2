define noundef range(i8 -1, 2) i8 @f3(i16 noundef signext %0, i16 noundef signext %1) {
  %3 = icmp slt i16 %0, %1
  %4 = select i1 %3, i8 -1, i8 1
  %5 = icmp eq i16 %0, %1
  %6 = select i1 %5, i8 0, i8 %4
  ret i8 %6
}
