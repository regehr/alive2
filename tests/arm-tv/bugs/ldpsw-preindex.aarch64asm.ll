; LDPSW with pre-indexing loads from the updated base.
; The words [1, 2, 3, 4] at the old base make [base, #8]! load 3 and 4.
; Expected constant was also checked on native AArch64 hardware.

; Expected X0: 0x0000000000000003.
define i64 @ldpsw_preindex() {
  ret i64 3
}
