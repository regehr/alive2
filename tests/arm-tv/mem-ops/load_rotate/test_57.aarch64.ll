@a = external global i57

declare i57 @llvm.fshr.i57 (i57 %a, i57 %b, i57 %c)

define i57 @f() {
  %1 = load i57, ptr @a, align 1
  %r = call i57 @llvm.fshr.i57(i57 %1, i57 %1, i57 1)
  ret i57 %r
}
