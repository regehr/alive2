@a = external global i83

declare i83 @llvm.fshr.i83 (i83 %a, i83 %b, i83 %c)

define void @f() {
  %1 = load i83, ptr @a, align 1
  %r = call i83 @llvm.fshr.i83(i83 %1, i83 %1, i83 1)
  store i83 %r, ptr @a, align 1
  ret void
}
