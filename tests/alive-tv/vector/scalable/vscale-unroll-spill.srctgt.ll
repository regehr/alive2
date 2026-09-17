; TEST-ARGS: --max-vscale=4 --src-unroll=4 --tgt-unroll=4
; CHECK: %r#ptr#2 = alloca i32 8
; CHECK: Checking vscale = 4
; CHECK: Transformation seems to be correct!
; CHECK-NOT: ERROR:

; Multiple dominating loop exits make the unroller spill the vector through
; memory. Each typing needs fresh preprocessing with its concrete byte size.
define <vscale x 2 x i32> @src(<vscale x 2 x i32> %x) {
entry:
  br label %for.cond

for.cond:
  %i = phi i32 [ 0, %entry ], [ %inc1, %for.body ]
  %r = phi <vscale x 2 x i32> [ %x, %entry ], [ %inc, %for.body ]
  %cmp = icmp uge i32 %i, 4
  br i1 %cmp, label %for.end2, label %for.body

for.body:
  %inc = add <vscale x 2 x i32> %r, zeroinitializer
  %inc1 = add i32 %i, 1
  %cmp2 = icmp eq i32 %inc1, 4
  br i1 %cmp2, label %for.end1, label %for.cond

for.end1:
  br label %pre.exit

for.end2:
  br label %pre.exit

pre.exit:
  br label %exit

exit:
  ret <vscale x 2 x i32> %r
}


define <vscale x 2 x i32> @tgt(<vscale x 2 x i32> %x) {
  ret <vscale x 2 x i32> %x
}
