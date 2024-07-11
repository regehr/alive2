@a = external global i37

declare i37 @llvm.fshr.i37 (i37 %a, i37 %b, i37 %c)

define void @f() {
  %1 = load i37, ptr @a, align 1
  %r = call i37 @llvm.fshr.i37(i37 %1, i37 %1, i37 1)
  store i37 %r, ptr @a, align 1
  ret void
}
