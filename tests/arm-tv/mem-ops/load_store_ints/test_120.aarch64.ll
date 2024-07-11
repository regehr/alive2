@a = external global i120

declare i120 @llvm.fshr.i120 (i120 %a, i120 %b, i120 %c)

define void @f() {
  %1 = load i120, ptr @a, align 1
  %r = call i120 @llvm.fshr.i120(i120 %1, i120 %1, i120 1)
  store i120 %r, ptr @a, align 1
  ret void
}
