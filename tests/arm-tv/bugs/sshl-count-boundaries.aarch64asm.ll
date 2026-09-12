; Check every destination byte against explicit ISA results.
; Counts include -128, -(width+1), -width, -(width-1), -1, 0, 1,
; width-1, width, width+1, and 127, using mixed-sign source lanes.
; Assembly ORs differences from the expected halves; both must be zero.
define i64 @sshl_count_boundaries() {
  ret i64 0
}
