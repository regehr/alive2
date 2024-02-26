; Function Attrs: nounwind
define void @f(i64 %0) {
  %2 = alloca i128, i64 %0, align 8
  %3 = load i128, ptr %2, align 4
  ret void
}