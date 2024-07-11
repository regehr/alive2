define i55 @f(ptr %0, i32 %z) {
  %2 = tail call signext i55 %0(i32 %z)
  ret i55 %2
}
