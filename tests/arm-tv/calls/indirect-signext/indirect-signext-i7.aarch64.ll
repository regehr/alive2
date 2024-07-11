define i7 @f(ptr %0, i32 %z) {
  %2 = tail call signext i7 %0(i32 %z)
  ret i7 %2
}
