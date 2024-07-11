@a = external global i74

declare i74 @llvm.fshr.i74 (i74 %a, i74 %b, i74 %c)

define void @f() {
  %1 = load i74, ptr @a, align 1
  %r = call i74 @llvm.fshr.i74(i74 %1, i74 %1, i74 1)
  store i74 %r, ptr @a, align 1
  ret void
}
