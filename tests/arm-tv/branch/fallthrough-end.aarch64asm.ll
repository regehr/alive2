; CHECK: ERROR: Assembly falls through past the end of the function
; CHECK-NOT: Transformation seems to be correct!

; The shared lifter must not supply a missing return on AArch64 either.
define i64 @fallthrough_end() {
  ret i64 1
}
