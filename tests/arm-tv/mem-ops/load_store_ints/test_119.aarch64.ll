@a = external global i119

declare i119 @llvm.fshr.i119 (i119 %a, i119 %b, i119 %c)

define void @f() {
  %1 = load i119, ptr @a, align 1
  %r = call i119 @llvm.fshr.i119(i119 %1, i119 %1, i119 1)
  store i119 %r, ptr @a, align 1
  ret void
}
