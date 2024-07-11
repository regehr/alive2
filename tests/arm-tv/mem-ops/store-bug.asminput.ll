; CHECK: 1 incorrect

define void @f(i3 %x, ptr %p) {
  store i3 %x, ptr %p
  ret void
}
