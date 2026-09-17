declare i32 @llvm.vscale.i32()

define i32 @src() vscale_range(2) {
  %v = call i32 @llvm.vscale.i32()
  ret i32 %v
}

define i32 @tgt() vscale_range(2) {
  ret i32 2
}


; TEST-ARGS: --max-vscale=2
