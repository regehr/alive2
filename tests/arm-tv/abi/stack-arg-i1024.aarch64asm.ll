; CHECK: 1 correct transformations

; Sixteen limbs: x0-x7 and then 64 bytes of stack.
define i64 @f(i1024 %w) {
  %s = lshr i1024 %w, 960
  %t = trunc i1024 %s to i64
  ret i64 %t
}
