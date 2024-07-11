@a = external global i68

declare i68 @llvm.fshr.i68 (i68 %a, i68 %b, i68 %c)

define void @f() {
  %1 = load i68, ptr @a, align 1
  %r = call i68 @llvm.fshr.i68(i68 %1, i68 %1, i68 1)
  store i68 %r, ptr @a, align 1
  ret void
}
