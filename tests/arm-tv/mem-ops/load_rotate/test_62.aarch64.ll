@a = external global i62

declare i62 @llvm.fshr.i62 (i62 %a, i62 %b, i62 %c)

define i62 @f() {
  %1 = load i62, ptr @a, align 1
  %r = call i62 @llvm.fshr.i62(i62 %1, i62 %1, i62 1)
  ret i62 %r
}
