@a = external global i87

declare i87 @llvm.fshr.i87 (i87 %a, i87 %b, i87 %c)

define void @f() {
  %1 = load i87, ptr @a, align 1
  %r = call i87 @llvm.fshr.i87(i87 %1, i87 %1, i87 1)
  store i87 %r, ptr @a, align 1
  ret void
}
