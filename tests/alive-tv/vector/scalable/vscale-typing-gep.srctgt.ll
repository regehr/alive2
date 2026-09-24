; TEST-ARGS: --max-vscale=4
; CHECK: Checking vscale = 1
; CHECK: Checking vscale = 2
; CHECK: Checking vscale = 4
; CHECK: Transformation seems to be correct!
; CHECK-NOT: ERROR:

; The scalable stride changes with each assignment; the element stride is 4.
define ptr @src(ptr %p) {
  %q = getelementptr <vscale x 2 x i32>, ptr %p, i64 1, i64 1
  ret ptr %q
}

define ptr @tgt(ptr %p) {
  %v = call i64 @llvm.vscale.i64()
  %size = mul i64 %v, 8
  %offset = add i64 %size, 4
  %q = getelementptr i8, ptr %p, i64 %offset
  ret ptr %q
}
