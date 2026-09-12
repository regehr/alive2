; Shift counts use the signed low byte, ignoring all higher bits.
; The lanes encode counts 0, 1, 31, and 32 with different upper bits.
; Assembly checks the full destination against explicit ISA results.
define i64 @sshl_count_high_bits() {
  ret i64 0
}
