@a = external global i97

declare i97 @llvm.fshr.i97 (i97 %a, i97 %b, i97 %c)

define void @f() {
  %1 = load i97, ptr @a, align 1
  %r = call i97 @llvm.fshr.i97(i97 %1, i97 %1, i97 1)
  store i97 %r, ptr @a, align 1
  ret void
}
