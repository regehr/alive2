define ptr @f(ptr %0, i16 %1) {
  %3 = getelementptr inbounds i16, ptr %0, i64 -128
  store i16 %1, ptr %0, align 4
  ret ptr %3
}