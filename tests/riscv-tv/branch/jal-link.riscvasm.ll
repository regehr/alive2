; CHECK: ERROR: JAL with a nonzero destination register is not supported
; CHECK-NOT: Transformation seems to be correct!

; JAL overwrites t0 with the link address, so this cannot return zero.
define i64 @jal_link() {
  ret i64 0
}
