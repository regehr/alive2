; Check RISC-V M extension division/remainder against explicit ISA results.
; See https://docs.riscv.org/reference/isa/unpriv/m-st-ext.html
;
; Assembly ORs together (actual XOR expected) for 18 full-width results.
; Zero means every result matched, including sign extension of unsigned
; word operations. This avoids LLVM division/remainder on undefined inputs.
define i64 @div_rem_edge_cases() {
  ret i64 0
}
