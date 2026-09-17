; TEST-ARGS: --max-vscale=2

; The vector stride scales, while the scalar element stride remains 4 bytes.
define ptr @src(ptr %p) {
  %q = getelementptr <vscale x 2 x i32>, ptr %p, i64 1, i64 1
  ret ptr %q
}

define ptr @tgt(ptr %p) {
  %vs = call i64 @llvm.vscale.i64()
  %stride = mul i64 %vs, 8
  %offset = add i64 %stride, 4
  %q = getelementptr i8, ptr %p, i64 %offset
  ret ptr %q
}
