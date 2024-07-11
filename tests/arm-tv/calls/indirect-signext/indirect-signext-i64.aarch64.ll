define i64 @f(ptr %0, i32 %z) {
  %2 = tail call signext i64 %0(i32 %z)
  ret i64 %2
}
