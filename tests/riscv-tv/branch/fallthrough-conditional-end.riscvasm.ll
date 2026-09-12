; CHECK: ERROR: Assembly falls through past the end of the function
; CHECK-NOT: Transformation seems to be correct!

; When x is zero the final branch falls out of the function, not to .Lreturn.
define i64 @fallthrough_conditional_end(i64 %x) {
  ret i64 42
}
