define ptr @f(ptr %0, ptr %1) {
  %3 = getelementptr inbounds i32, ptr %0, i64 1
  %4 = load i32, ptr %3, align 4
  store i32 %4, ptr %1, align 4
  ret ptr %3
}