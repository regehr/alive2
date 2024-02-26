define ptr @f(ptr %0, ptr %1) {
  %3 = getelementptr inbounds i8, ptr %0, i64 1
  %4 = load i8, ptr %3, align 4
  %5 = zext i8 %4 to i64
  store i64 %5, ptr %1, align 4
  ret ptr %3
}