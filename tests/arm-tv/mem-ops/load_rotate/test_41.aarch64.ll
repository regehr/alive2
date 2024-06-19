@a = external global i41

declare i41 @llvm.fshr.i41 (i41 %a, i41 %b, i41 %c)

define i41 @f() {
  %1 = load i41, ptr @a, align 1
  %r = call i41 @llvm.fshr.i41(i41 %1, i41 %1, i41 1)
  ret i41 %r
}
