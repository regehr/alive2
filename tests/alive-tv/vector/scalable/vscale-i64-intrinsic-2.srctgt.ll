declare i64 @llvm.vscale.i64()

define i64 @src() vscale_range(2) {
  %v = call i64 @llvm.vscale.i64()
  ret i64 %v
}

define i64 @tgt() vscale_range(2) {
  ret i64 2
}


; TEST-ARGS: --max-vscale=2
