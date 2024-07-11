@a = external global i75

declare i75 @llvm.fshr.i75 (i75 %a, i75 %b, i75 %c)

define void @f() {
  %1 = load i75, ptr @a, align 1
  %r = call i75 @llvm.fshr.i75(i75 %1, i75 %1, i75 1)
  store i75 %r, ptr @a, align 1
  ret void
}
