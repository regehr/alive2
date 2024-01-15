define dso_local i32 @f(ptr %0, i32 %1) {
  %3 = sext i32 %1 to i64
  %4 = getelementptr inbounds i16, ptr %0, i64 %3
  %5 = load i16, ptr %4, align 2
  %6 = load i16, ptr %4, align 4
  %7 = sext i16 %5 to i64
  %8 = getelementptr inbounds i64, ptr %0, i64 %7
  %9 = load i64, ptr %8, align 16
  %10 = load i64, ptr %8, align 32
  %11 = trunc i64 %9 to i32
  ret i32 %11
}