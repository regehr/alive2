define i64 @f1() {
  %1 = alloca [16 x i64], align 4
  %2 = load i64, ptr %1, align 4
  ret i64 %2
}