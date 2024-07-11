define i38 @f(ptr %0, i32 %z) {
  %2 = tail call signext i38 %0(i32 %z)
  ret i38 %2
}
