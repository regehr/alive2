@a = external global i25

declare i25 @llvm.fshr.i25 (i25 %a, i25 %b, i25 %c)

define void @f() {
  %1 = load i25, ptr @a, align 1
  %r = call i25 @llvm.fshr.i25(i25 %1, i25 %1, i25 1)
  store i25 %r, ptr @a, align 1
  ret void
}
