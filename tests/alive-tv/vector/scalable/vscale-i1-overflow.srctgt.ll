; TEST-ARGS: --max-vscale=2
; ERROR: Target is more poisonous than source
; CHECK: Checking vscale = 2
; CHECK-NOT: Transformation seems to be correct!

; vscale=2 does not fit in i1, so the intrinsic produces poison, not zero.
define i1 @src() {
  ret i1 true
}

define i1 @tgt() {
  %v = call i1 @llvm.vscale.i1()
  ret i1 %v
}
