; TEST-ARGS: --max-vscale=4 --smt-verbose
; CHECK: Checking vscale = 4
; CHECK: Transformation seems to be correct!
; CHECK: SMT query (typing):
; CHECK: (declare-fun vscale () (_ BitVec 3))
; CHECK-NOT: ERROR:

; vscale is symbolic when choosing typings and concrete during verification.

define i32 @src(i32 %x) {
  %v = call i32 @llvm.vscale.i32()
  %r = mul i32 %x, %v
  ret i32 %r
}
define i32 @tgt(i32 %x) {
  %v = call i32 @llvm.vscale.i32()
  %neg = sub i32 0, %x
  %prod = mul i32 %neg, %v
  %r = sub i32 0, %prod
  ret i32 %r
}
