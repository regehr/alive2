@a = external global i22

declare i22 @llvm.fshr.i22 (i22 %a, i22 %b, i22 %c)

define void @f() {
  %1 = load i22, ptr @a, align 1
  %r = call i22 @llvm.fshr.i22(i22 %1, i22 %1, i22 1)
  store i22 %r, ptr @a, align 1
  ret void
}
