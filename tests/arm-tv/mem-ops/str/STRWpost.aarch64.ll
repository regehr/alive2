define ptr @store32(ptr %0, i32 %1, i32 %2) {
  %4 = getelementptr inbounds i32, ptr %0, i64 1
  store i32 %2, ptr %0, align 4
  ret ptr %4
}