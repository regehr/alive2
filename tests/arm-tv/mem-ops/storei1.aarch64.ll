define void @f(ptr %p, i1 %b) {
  store i1 %b, ptr %p
  ret void
}
