; ERROR: No single source execution matches the target's behavior

; The call to @g forces %x == 0, so the return value must be 0 as well.
declare void @g(i32)

define i32 @src() {
  %x = freeze i32 poison
  call void @g(i32 %x)
  ret i32 %x
}

define i32 @tgt() {
  call void @g(i32 0)
  ret i32 1
}
