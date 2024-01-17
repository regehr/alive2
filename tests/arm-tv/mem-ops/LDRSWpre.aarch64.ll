define ptr @f(ptr %0, ptr %1) {
  %3 = getelementptr inbounds i32, ptr %0, i64 1
  %4 = load i32, ptr %3, align 4
  %5 = sext i32 %4 to i64
  store i64 %5, ptr %1, align 8
  ret ptr %3
}