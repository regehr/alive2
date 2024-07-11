define i45 @f(ptr %0, i32 %z) {
  %2 = tail call zeroext i45 %0(i32 %z)
  ret i45 %2
}
