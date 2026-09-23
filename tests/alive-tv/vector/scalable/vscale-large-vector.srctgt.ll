; TEST-ARGS: --quiet --disable-undef-input --max-vscale=1024
; CHECK: Checking vscale = 1024
; CHECK: Transformation seems to be correct!
; CHECK-NOT: ERROR:

; The realized vector has 32768 lanes, which needs all 16 count bits.
define i1 @src() vscale_range(1024) {
  ret i1 false
}
define i1 @tgt() vscale_range(1024) {
  %r = extractelement <vscale x 32 x i1> zeroinitializer, i32 32767
  ret i1 %r
}
