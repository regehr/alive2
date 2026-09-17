; TEST-ARGS: --disable-undef-input --max-vscale=64
; ERROR: Value mismatch
; CHECK: Checking vscale = 32
; CHECK: Checking vscale = 64
; CHECK-NOT: Transformation seems to be correct!

; Incorrectly assume that every valid vector index fits in six bits.
; This refines src when vscale <= 32, but fails starting at vscale = 64,
; where indices 64 through 127 are valid and the mask changes them.
define i8 @src(<vscale x 2 x i8> noundef %v, i32 noundef %idx) {
  %r = extractelement <vscale x 2 x i8> %v, i32 %idx
  ret i8 %r
}

define i8 @tgt(<vscale x 2 x i8> noundef %v, i32 noundef %idx) {
  %masked = and i32 %idx, 63
  %r = extractelement <vscale x 2 x i8> %v, i32 %masked
  ret i8 %r
}
