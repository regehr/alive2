define ptr @f(ptr %0, ptr %1) {
  %3 = load i8, ptr %0, align 1
  %4 = getelementptr inbounds i8, ptr %0, i64 1
  %5 = sext i8 %3 to i32
  store i32 %5, ptr %1, align 4
  ret ptr %4
}