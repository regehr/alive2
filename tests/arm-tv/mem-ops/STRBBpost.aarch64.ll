define ptr @f(ptr %0) {
  %2 = alloca [32 x i8], align 1
  %3 = load i8, ptr %0, align 1
  store i8 %3, ptr %2, align 1
  ret ptr %2
}