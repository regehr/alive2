define i10 @f(ptr %0, i32 %z) {
  %2 = tail call zeroext i10 %0(i32 %z)
  ret i10 %2
}
