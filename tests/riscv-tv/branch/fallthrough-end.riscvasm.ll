; CHECK: ERROR: Assembly falls through past the end of the function
; CHECK-NOT: Transformation seems to be correct!

; Reaching the end of the assembly must not supply a missing return.
define i64 @fallthrough_end() {
  ret i64 1
}
