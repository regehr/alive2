define i32 @f(ptr %0) {
  %2 = getelementptr inbounds i8, ptr %0, i32 -256
  %3 = load i16, ptr %2, align 2
  %4 = sext i16 %3 to i32
  ret i32 %4
}