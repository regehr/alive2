; Function Attrs: nounwind
define i64 @f(ptr %0) {
  %2 = load i32, ptr %0, align 4
  %3 = getelementptr inbounds i32, ptr %0, i64 1
  %4 = load i32, ptr %3, align 4
  %5 = sext i32 %2 to i64
  %6 = sext i32 %4 to i64
  %7 = add nsw i64 %6, %5
  ret i64 %7
}