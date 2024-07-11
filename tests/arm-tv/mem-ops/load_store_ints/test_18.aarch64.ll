@a = external global i18

declare i18 @llvm.fshr.i18 (i18 %a, i18 %b, i18 %c)

define void @f() {
  %1 = load i18, ptr @a, align 1
  %r = call i18 @llvm.fshr.i18(i18 %1, i18 %1, i18 1)
  store i18 %r, ptr @a, align 1
  ret void
}
