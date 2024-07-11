@a = external global i21

declare i21 @llvm.fshr.i21 (i21 %a, i21 %b, i21 %c)

define void @f() {
  %1 = load i21, ptr @a, align 1
  %r = call i21 @llvm.fshr.i21(i21 %1, i21 %1, i21 1)
  store i21 %r, ptr @a, align 1
  ret void
}
