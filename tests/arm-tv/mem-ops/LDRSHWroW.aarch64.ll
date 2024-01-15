define i32 @f(ptr %0, i32 %1) {
  %3 = getelementptr inbounds i16, ptr %0, i32 %1
  %4 = load i16, ptr %3, align 2
  %5 = sext i16 %4 to i32
  ret i32 %5
}