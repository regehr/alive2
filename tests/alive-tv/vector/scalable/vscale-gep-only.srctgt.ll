; TEST-ARGS: --max-vscale=2
; CHECK: Checking vscale = 1
; CHECK: Checking vscale = 2
; CHECK-NOT: Transformation seems to be correct!
; ERROR: Value mismatch

; There are no scalable SSA values and no llvm.vscale intrinsic.
define ptr @src(ptr %p) {
  %q = getelementptr <vscale x 2 x i32>, ptr %p, i64 1
  ret ptr %q
}
define ptr @tgt(ptr %p) {
  %q = getelementptr i8, ptr %p, i64 8
  ret ptr %q
}
