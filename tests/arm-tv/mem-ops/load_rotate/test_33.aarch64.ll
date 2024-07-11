@a = external global i33

declare i33 @llvm.fshr.i33 (i33 %a, i33 %b, i33 %c)

define i33 @f() {
  %1 = load i33, ptr @a, align 1
  %r = call i33 @llvm.fshr.i33(i33 %1, i33 %1, i33 1)
  ret i33 %r
}
