define i32 @f() {
  %1 = alloca [32 x i8], align 4
  %2 = load i8, ptr %1, align 1
  %3 = zext i8 %2 to i32
  ret i32 %3
}