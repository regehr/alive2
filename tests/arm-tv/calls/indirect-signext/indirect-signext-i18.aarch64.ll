define i18 @f(ptr %0, i32 %z) {
  %2 = tail call signext i18 %0(i32 %z)
  ret i18 %2
}
