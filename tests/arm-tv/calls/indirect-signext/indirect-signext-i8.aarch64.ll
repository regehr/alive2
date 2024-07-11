define i8 @f(ptr %0, i32 %z) {
  %2 = tail call signext i8 %0(i32 %z)
  ret i8 %2
}
