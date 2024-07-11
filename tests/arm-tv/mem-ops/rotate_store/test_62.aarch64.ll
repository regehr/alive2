@a = external global i62

declare i62 @llvm.fshr.i62 (i62 %a, i62 %b, i62 %c)

define void @f(i62 %arg) {
  %r = call i62 @llvm.fshr.i62(i62 %arg, i62 %arg, i62 1)
  store i62 %r, ptr @a, align 1
  ret void
}
