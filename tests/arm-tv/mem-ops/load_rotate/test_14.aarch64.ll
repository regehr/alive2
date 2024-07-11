@a = external global i14

declare i14 @llvm.fshr.i14 (i14 %a, i14 %b, i14 %c)

define i14 @f() {
  %1 = load i14, ptr @a, align 1
  %r = call i14 @llvm.fshr.i14(i14 %1, i14 %1, i14 1)
  ret i14 %r
}
