; TEST-ARGS: --max-vscale=0
; SKIP-IDENTITY
; ERROR: --max-vscale must be greater than zero

define i32 @src() {
  ret i32 0
}
define i32 @tgt() {
  ret i32 0
}
