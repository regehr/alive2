define ptr @f(ptr %0, i16 %1) {
  %3 = getelementptr inbounds i8, ptr %0, i32 -4
  store i16 %1, ptr %3, align 2
  ret ptr %3
}