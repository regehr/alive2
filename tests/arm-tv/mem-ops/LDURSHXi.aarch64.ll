define dso_local i64 @f(ptr nocapture readonly %0) {
  %2 = getelementptr inbounds i8, ptr %0, i64 1
  %3 = load i16, ptr %2, align 2
  %4 = sext i16 %3 to i64
  ret i64 %4
}