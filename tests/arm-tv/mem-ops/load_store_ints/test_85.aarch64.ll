@a = external global i85

declare i85 @llvm.fshr.i85 (i85 %a, i85 %b, i85 %c)

define void @f() {
  %1 = load i85, ptr @a, align 1
  %r = call i85 @llvm.fshr.i85(i85 %1, i85 %1, i85 1)
  store i85 %r, ptr @a, align 1
  ret void
}
