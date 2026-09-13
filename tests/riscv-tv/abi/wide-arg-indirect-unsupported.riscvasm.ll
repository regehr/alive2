; ERROR: Unsupported function argument: a 192-bit value would be passed at INDIRECT r0, which we don't support yet

; RV64 passes anything wider than 2*XLEN by reference. We model where
; the pointer goes, but do not lift the buffer behind it yet.
define i64 @f(i192 %x) {
  %t = trunc i192 %x to i64
  ret i64 %t
}
