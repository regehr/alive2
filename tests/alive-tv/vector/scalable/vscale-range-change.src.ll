; TEST-ARGS: --max-vscale=4 --always-verify=false --bidirectional
; ERROR: Unsupported change to vscale_range
; CHECK-NOT: Transformation seems to be correct!
; CHECK-NOT: Checking vscale =

; The printed Alive IR is identical: reject the changed range before the
; syntactic-equality shortcut, even without scalable vector operations.
define i32 @f(i32 %x) vscale_range(1,4) {
  ret i32 %x
}
