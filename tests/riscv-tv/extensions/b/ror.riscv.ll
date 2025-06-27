define i64 @ror_intr(i64 %x, i64 %amt) {
entry:
  %r = call i64 @llvm.fshr.i64(i64 %x, i64 %x, i64 %amt)
  ret i64 %r
}
declare i64 @llvm.fshr.i64(i64 %a, i64 %b, i64 %amt)
