define ptr @f(ptr %0, ptr %1) {
  %3 = load i16, ptr %0, align 2
  %4 = getelementptr inbounds i16, ptr %0, i64 1
  %5 = sext i16 %3 to i64
  store i64 %5, ptr %1, align 8
  ret ptr %4
}