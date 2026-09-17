declare i32 @llvm.vscale.i32()

define i32 @src() vscale_range(4) {
  %v = call i32 @llvm.vscale.i32()
  ret i32 %v
}

define i32 @tgt() vscale_range(4) {
  ret i32 4
}


; TEST-ARGS: --max-vscale=4
