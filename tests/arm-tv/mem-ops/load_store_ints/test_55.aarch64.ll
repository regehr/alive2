@a = external global i55

declare i55 @llvm.fshr.i55 (i55 %a, i55 %b, i55 %c)

define void @f() {
  %1 = load i55, ptr @a, align 1
  %r = call i55 @llvm.fshr.i55(i55 %1, i55 %1, i55 1)
  store i55 %r, ptr @a, align 1
  ret void
}
