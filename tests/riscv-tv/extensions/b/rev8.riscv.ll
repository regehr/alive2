define i32 @foo(i32 %x) {
entry:
  %r = call i32 @llvm.bswap.i32(i32 %x)
  ret i32 %r
}

declare i32 @llvm.bswap.i32(i32)
