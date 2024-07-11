define i23 @f(ptr %0, i32 %z) {
  %2 = tail call signext i23 %0(i32 %z)
  ret i23 %2
}
