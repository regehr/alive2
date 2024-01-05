define ptr @store64(ptr %0, i32 %1, i64 %2) {
  %4 = getelementptr inbounds i32, ptr %0, i64 1
  store i64 %2, ptr %0, align 4
  ret ptr %4
}