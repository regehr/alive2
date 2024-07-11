@a = external global i89

declare i89 @llvm.fshr.i89 (i89 %a, i89 %b, i89 %c)

define void @f() {
  %1 = load i89, ptr @a, align 1
  %r = call i89 @llvm.fshr.i89(i89 %1, i89 %1, i89 1)
  store i89 %r, ptr @a, align 1
  ret void
}
