; TEST-ARGS: --max-vscale=1

define i1 @src() {
  %v = call i1 @llvm.vscale.i1()
  ret i1 %v
}

define i1 @tgt() {
  ret i1 true
}
