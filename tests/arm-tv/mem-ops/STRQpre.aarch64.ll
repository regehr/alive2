define void @f(ptr %0, ptr %1) {
  %3 = load fp128, ptr %1, align 16
  %4 = fneg fp128 %3
  store fp128 %4, ptr %0, align 16
  ret void
}