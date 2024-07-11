@a = external global i118

declare i118 @llvm.fshr.i118 (i118 %a, i118 %b, i118 %c)

define void @f() {
  %1 = load i118, ptr @a, align 1
  %r = call i118 @llvm.fshr.i118(i118 %1, i118 %1, i118 1)
  store i118 %r, ptr @a, align 1
  ret void
}
