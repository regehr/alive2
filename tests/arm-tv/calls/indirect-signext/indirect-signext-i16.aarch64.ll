define i16 @f(ptr %0, i32 %z) {
  %2 = tail call signext i16 %0(i32 %z)
  ret i16 %2
}
