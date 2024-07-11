@a = external global i91

declare i91 @llvm.fshr.i91 (i91 %a, i91 %b, i91 %c)

define void @f() {
  %1 = load i91, ptr @a, align 1
  %r = call i91 @llvm.fshr.i91(i91 %1, i91 %1, i91 1)
  store i91 %r, ptr @a, align 1
  ret void
}
