@a = external global i59

declare i59 @llvm.fshr.i59 (i59 %a, i59 %b, i59 %c)

define i59 @f() {
  %1 = load i59, ptr @a, align 1
  %r = call i59 @llvm.fshr.i59(i59 %1, i59 %1, i59 1)
  ret i59 %r
}
