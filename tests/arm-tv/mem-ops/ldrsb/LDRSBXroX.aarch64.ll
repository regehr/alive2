define dso_local i64 @f(ptr nocapture readonly %0) {
  %2 = getelementptr inbounds i8, ptr %0, i64 99999000
  %3 = load i8, ptr %2, align 1
  %4 = sext i8 %3 to i64
  ret i64 %4
}