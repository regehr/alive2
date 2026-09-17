; TEST-ARGS: --max-vscale=4 --tgt-unroll=2
; SKIP-IDENTITY
; CHECK: Checking vscale = 2
; CHECK-NOT: Transformation seems to be correct!
; ERROR: The target program doesn't reach a return instruction.

define i32 @src() {
  ret i32 0
}
define i32 @tgt() {
entry:
  %v = call i32 @llvm.vscale.i32()
  %n = mul i32 %v, 2
  br label %loop
loop:
  %i = phi i32 [ 0, %entry ], [ %next, %loop ]
  %next = add i32 %i, 1
  %again = icmp ult i32 %next, %n
  br i1 %again, label %loop, label %exit
exit:
  ret i32 0
}
