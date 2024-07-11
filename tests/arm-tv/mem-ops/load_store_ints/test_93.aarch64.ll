@a = external global i93

declare i93 @llvm.fshr.i93 (i93 %a, i93 %b, i93 %c)

define void @f() {
  %1 = load i93, ptr @a, align 1
  %r = call i93 @llvm.fshr.i93(i93 %1, i93 %1, i93 1)
  store i93 %r, ptr @a, align 1
  ret void
}
