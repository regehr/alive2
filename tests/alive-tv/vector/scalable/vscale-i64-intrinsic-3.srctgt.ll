declare i64 @llvm.vscale.i64()

define i64 @src() vscale_range(4) {
  %v = call i64 @llvm.vscale.i64()
  ret i64 %v
}

define i64 @tgt() vscale_range(4) {
  ret i64 4
}


; TEST-ARGS: --max-vscale=4
