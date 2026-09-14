; CHECK: 1 correct transformations

; RV64 splits a 2*XLEN scalar asymmetrically when exactly one argument
; register is left: the low limb goes in a7 and the high limb to 0(sp).
; AArch64 pushes both halves to the stack in the same situation.
define i64 @f(i64 %a,i64 %b,i64 %c,i64 %d,i64 %e,i64 %g,i64 %h, i128 %w) {
  %s = lshr i128 %w, 64
  %t = trunc i128 %s to i64
  ret i64 %t
}
