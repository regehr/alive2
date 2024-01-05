define void @storei64stk(i64 %0) {
  %2 = alloca i64, align 16
  store i64 %0, ptr %2, align 16
  ret void
}