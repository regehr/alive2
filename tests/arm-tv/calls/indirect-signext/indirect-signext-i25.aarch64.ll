define i25 @f(ptr %0, i32 %z) {
  %2 = tail call signext i25 %0(i32 %z)
  ret i25 %2
}
