define dso_local signext i16 @f(ptr nocapture readonly %0) {
  %2 = getelementptr inbounds i8, ptr %0, i64 99999000
  %3 = load i64, ptr %2, align 8
  %4 = trunc i64 %3 to i16
  ret i16 %4
}