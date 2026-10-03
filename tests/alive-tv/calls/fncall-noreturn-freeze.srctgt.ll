; ERROR: No single source execution matches the target's behavior

; Source either returns 0 or doesn't return; target returns 1.
declare void @h() noreturn

define i32 @src() {
  %x = freeze i1 poison
  br i1 %x, label %a, label %b
a:
  ret i32 0
b:
  call void @h() noreturn
  unreachable
}

define i32 @tgt() {
  ret i32 1
}
