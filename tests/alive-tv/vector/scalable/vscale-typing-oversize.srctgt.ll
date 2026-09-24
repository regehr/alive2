; TEST-ARGS: --max-vscale=65536
; SKIP-IDENTITY
; ERROR: Vector type is too large
; CHECK-NOT: Transformation seems to be correct!

; An unsupported assignment must fail explicitly, rather than disappear from
; the typing domain or wrap the element count to zero.
define i32 @src(<vscale x 2 x i8> %x) vscale_range(65536) {
  ret i32 0
}
define i32 @tgt(<vscale x 2 x i8> %x) vscale_range(65536) {
  ret i32 0
}
