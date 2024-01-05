define void @storei32stk(i32 %0) {
  %2 = alloca i32, align 16
  store i32 %0, ptr %2, align 16
  ret void
}
