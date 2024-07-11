define void @f(ptr %p) {
entry:
  call void %p(i32 0)
  ret void
}
