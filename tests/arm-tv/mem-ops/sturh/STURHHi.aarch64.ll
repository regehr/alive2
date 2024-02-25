define void @f(ptr %0) {
  store i32 5, ptr %0, align 4
  store i8 7, ptr %0, align 1
  store i16 -30062, ptr %0, align 2
  %2 = getelementptr inbounds i8, ptr %0, i64 2
  store i8 25, ptr %2, align 1
  %3 = getelementptr inbounds i8, ptr %0, i64 3
  store i8 47, ptr %3, align 1
  %4 = getelementptr inbounds i8, ptr %0, i64 1
  store i16 2020, ptr %4, align 1
  ret void
}