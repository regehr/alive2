define i28 @f(ptr %0, i32 %z) {
  %2 = tail call signext i28 %0(i32 %z)
  ret i28 %2
}
