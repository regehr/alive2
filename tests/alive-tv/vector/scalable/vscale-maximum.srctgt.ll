; TEST-ARGS: --max-vscale=4095
; CHECK: Checking vscale = 1024
; CHECK: Checking vscale = 2048
; CHECK: Transformation seems to be correct! (all applicable vscale values up to 4095)
; CHECK-NOT: Checking vscale = 4096
; CHECK-NOT: ERROR:

define i1 @src() {
  ret i1 true
}
define i1 @tgt() {
  %v = call i32 @llvm.vscale.i32()
  %r = icmp ule i32 %v, 2048
  ret i1 %r
}
