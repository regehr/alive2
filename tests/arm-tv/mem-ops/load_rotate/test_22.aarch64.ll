@a = external global i22

declare i22 @llvm.fshr.i22 (i22 %a, i22 %b, i22 %c)

define i22 @f() {
  %1 = load i22, ptr @a, align 1
  %r = call i22 @llvm.fshr.i22(i22 %1, i22 %1, i22 1)
  ret i22 %r
}
