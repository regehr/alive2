; CHECK: ERROR: JAL with a nonzero destination register is not supported
; CHECK-NOT: Transformation seems to be correct!

; The implicit ra destination also requires unsupported link-address semantics.
define i64 @jal_ra() {
  ret i64 42
}
