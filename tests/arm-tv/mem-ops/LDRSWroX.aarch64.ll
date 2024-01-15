define i64 @f(ptr %0) {
  %2 = getelementptr inbounds i32, ptr %0, i64 1039992
  %3 = load i32, ptr %2, align 2
  %4 = sext i32 %3 to i64
  ret i64 %4
}