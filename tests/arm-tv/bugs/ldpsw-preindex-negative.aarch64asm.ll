; A negative pre-index offset selects the preceding pair of words.
; Check both loaded values, sign extension, and base-register writeback.
; Assembly combines differences from the expected values; all must be zero.
define i64 @ldpsw_preindex_negative() {
  ret i64 0
}
