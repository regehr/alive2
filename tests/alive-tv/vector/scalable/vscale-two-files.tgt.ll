define i32 @f(i32 %x) {
  %v = call i32 @llvm.vscale.i32()
  %r = add i32 %v, %x
  ret i32 %r
}
