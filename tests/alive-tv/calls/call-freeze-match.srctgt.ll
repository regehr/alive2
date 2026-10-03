declare void @g(i32)

define i32 @src() {
  %x = freeze i32 poison
  call void @g(i32 %x)
  ret i32 %x
}

define i32 @tgt() {
  call void @g(i32 7)
  ret i32 7
}
