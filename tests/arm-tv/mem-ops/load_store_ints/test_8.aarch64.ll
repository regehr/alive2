@a = external global i8

declare i8 @llvm.fshr.i8 (i8 %a, i8 %b, i8 %c)

define void @f() {
  %1 = load i8, ptr @a, align 1
  %r = call i8 @llvm.fshr.i8(i8 %1, i8 %1, i8 1)
  store i8 %r, ptr @a, align 1
  ret void
}
