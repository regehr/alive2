define i31 @f(ptr %0, i32 %z) {
  %2 = tail call signext i31 %0(i32 %z)
  ret i31 %2
}
