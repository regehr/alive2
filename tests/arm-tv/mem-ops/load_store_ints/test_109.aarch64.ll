@a = external global i109

declare i109 @llvm.fshr.i109 (i109 %a, i109 %b, i109 %c)

define void @f() {
  %1 = load i109, ptr @a, align 1
  %r = call i109 @llvm.fshr.i109(i109 %1, i109 %1, i109 1)
  store i109 %r, ptr @a, align 1
  ret void
}
