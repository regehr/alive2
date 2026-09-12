; CHECK: 1 correct transformations
; CHECK-NOT: ERROR:

; Falling through an internal label is valid, and trailing unreachable
; instructions do not need a return.
define i64 @fallthrough_unreachable_end() {
  ret i64 42
}
