define i24 @f(ptr %0, i32 %z) {
  %2 = tail call signext i24 %0(i32 %z)
  ret i24 %2
}
