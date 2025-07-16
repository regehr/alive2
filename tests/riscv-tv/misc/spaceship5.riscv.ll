define noundef range(i8 -1, 2) i8 @f5(i32 noundef %0, i32 noundef %1) {
  %3 = icmp slt i32 %0, %1
  %4 = select i1 %3, i8 -1, i8 1
  %5 = icmp eq i32 %0, %1
  %6 = select i1 %5, i8 0, i8 %4
  ret i8 %6
}

