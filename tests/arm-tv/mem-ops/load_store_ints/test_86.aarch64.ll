@a = external global i86

declare i86 @llvm.fshr.i86 (i86 %a, i86 %b, i86 %c)

define void @f() {
  %1 = load i86, ptr @a, align 1
  %r = call i86 @llvm.fshr.i86(i86 %1, i86 %1, i86 1)
  store i86 %r, ptr @a, align 1
  ret void
}
