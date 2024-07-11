@a = external global i106

declare i106 @llvm.fshr.i106 (i106 %a, i106 %b, i106 %c)

define void @f() {
  %1 = load i106, ptr @a, align 1
  %r = call i106 @llvm.fshr.i106(i106 %1, i106 %1, i106 1)
  store i106 %r, ptr @a, align 1
  ret void
}
