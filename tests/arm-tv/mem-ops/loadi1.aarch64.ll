define i1 @f(ptr %p) {
  %v = load i1, ptr %p
  ret i1 %v
}
