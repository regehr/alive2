; Function Attrs: nounwind
define void @f() {
  %1 = alloca i8, align 1
  %2 = alloca i64, align 8
  %3 = load i64, ptr %2, align 8
  %4 = trunc i64 %3 to i8
  store i8 %4, ptr %1, align 1
  ret void
}