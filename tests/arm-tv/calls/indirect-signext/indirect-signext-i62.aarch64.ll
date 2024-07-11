define i62 @f(ptr %0, i32 %z) {
  %2 = tail call signext i62 %0(i32 %z)
  ret i62 %2
}
