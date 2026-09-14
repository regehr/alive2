; CHECK: 1 correct transformations

; Three limbs: AArch64 keeps even a 192-bit argument in registers.
define i64 @f(i192 %x) {
  %s = lshr i192 %x, 128
  %t = trunc i192 %s to i64
  ret i64 %t
}
