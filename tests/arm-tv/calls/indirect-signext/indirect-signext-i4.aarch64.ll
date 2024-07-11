define i4 @f(ptr %0, i32 %z) {
  %2 = tail call signext i4 %0(i32 %z)
  ret i4 %2
}
