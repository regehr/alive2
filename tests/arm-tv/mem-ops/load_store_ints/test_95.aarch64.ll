@a = external global i95

declare i95 @llvm.fshr.i95 (i95 %a, i95 %b, i95 %c)

define void @f() {
  %1 = load i95, ptr @a, align 1
  %r = call i95 @llvm.fshr.i95(i95 %1, i95 %1, i95 1)
  store i95 %r, ptr @a, align 1
  ret void
}
