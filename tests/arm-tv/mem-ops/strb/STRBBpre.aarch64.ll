; Function Attrs: nounwind
define ptr @f(ptr %0, i8 zeroext %1) {
  %3 = getelementptr inbounds i8, ptr %0, i64 16
  store i8 %1, ptr %3, align 1
  ret ptr %3
}