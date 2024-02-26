define ptr @f(ptr %0, ptr %1) {
  %3 = getelementptr inbounds i8, ptr %0, i64 1
  %4 = load i8, ptr %3, align 1
  %5 = sext i8 %4 to i64
  store i64 %5, ptr %1, align 8
  ret ptr %3
}