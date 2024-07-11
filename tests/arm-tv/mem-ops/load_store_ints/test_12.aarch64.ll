@a = external global i12

declare i12 @llvm.fshr.i12 (i12 %a, i12 %b, i12 %c)

define void @f() {
  %1 = load i12, ptr @a, align 1
  %r = call i12 @llvm.fshr.i12(i12 %1, i12 %1, i12 1)
  store i12 %r, ptr @a, align 1
  ret void
}
