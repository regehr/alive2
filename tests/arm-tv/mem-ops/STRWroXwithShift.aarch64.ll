define i32 @not_alone_in_block(i64 %0) {
  %2 = alloca [2 x i32], align 8
  store i64 0, ptr %2, align 8
  %3 = getelementptr inbounds [2 x i32], ptr %2, i64 0, i64 %0
  store i32 1, ptr %3, align 4
  %4 = load i32, ptr %2, align 8
  ret i32 %4
}