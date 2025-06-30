define i32 @ror_intr(i32 %x, i32 %amt) {
entry:
  %r = call i32 @llvm.fshr.i32(i32 %x, i32 %x, i32 %amt)
  ret i32 %r
}
declare i32 @llvm.fshr.i32(i32 %a, i32 %b, i32 %amt)
