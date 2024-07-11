@a = external global i3

declare i3 @llvm.fshr.i3 (i3 %a, i3 %b, i3 %c)

define void @f() {
  %1 = load i3, ptr @a, align 1
  %r = call i3 @llvm.fshr.i3(i3 %1, i3 %1, i3 1)
  store i3 %r, ptr @a, align 1
  ret void
}
