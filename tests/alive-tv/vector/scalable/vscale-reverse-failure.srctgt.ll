; TEST-ARGS: --max-vscale=4 --bidirectional
; CHECK: Checking reverse transformation at vscale = 2
; CHECK-NOT: Transformation seems to be correct!
; ERROR: Target is more poisonous than source

; Forward refinement holds at every scale. Reverse refinement fails at 2.
define i1 @src() {
  %v = call i1 @llvm.vscale.i1()
  ret i1 %v
}
define i1 @tgt() {
  ret i1 true
}
