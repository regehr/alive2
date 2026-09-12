; CHECK: 1 incorrect transformations

; A single-precision box is insufficient for a half-precision return.
; Reject even when the low bits happen to encode a canonical NaN.
define half @nanbox_return_half_wrong() {
  ret half 0xH7E00
}
