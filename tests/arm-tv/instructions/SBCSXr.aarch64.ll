define i32 @f(ptr %0, ptr %1) {
  %3 = load i72, ptr %0, align 4
  store i72 %3, ptr %1, align 4
  %4 = icmp ult i72 %3, 166153499473114484112
  %5 = zext i1 %4 to i32
  ret i32 %5
}