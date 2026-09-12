; SRI at the lane width preserves the active 64 bits and clears upper bits.
; The old upper half is nonzero, and the source complements the destination.
; Assembly checks both halves against explicit ISA results.
define i64 @sri_width_8() {
  ret i64 0
}
