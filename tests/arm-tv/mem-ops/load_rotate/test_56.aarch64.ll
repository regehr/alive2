@a = external global i56

declare i56 @llvm.fshr.i56 (i56 %a, i56 %b, i56 %c)

define i56 @f() {
  %1 = load i56, ptr @a, align 1
  %r = call i56 @llvm.fshr.i56(i56 %1, i56 %1, i56 1)
  ret i56 %r
}
