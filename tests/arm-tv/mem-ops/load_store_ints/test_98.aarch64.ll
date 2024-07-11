@a = external global i98

declare i98 @llvm.fshr.i98 (i98 %a, i98 %b, i98 %c)

define void @f() {
  %1 = load i98, ptr @a, align 1
  %r = call i98 @llvm.fshr.i98(i98 %1, i98 %1, i98 1)
  store i98 %r, ptr @a, align 1
  ret void
}
