define i32 @f1() {
  %1 = alloca [32 x i32], align 4
  %2 = load i32, ptr %1, align 4
  ret i32 %2
}