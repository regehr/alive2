@a = external global i18

declare i18 @llvm.fshr.i18 (i18 %a, i18 %b, i18 %c)

define void @f(i18 %arg) {
  %r = call i18 @llvm.fshr.i18(i18 %arg, i18 %arg, i18 1)
  store i18 %r, ptr @a, align 1
  ret void
}
