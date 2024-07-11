@a = external global i79

declare i79 @llvm.fshr.i79 (i79 %a, i79 %b, i79 %c)

define void @f() {
  %1 = load i79, ptr @a, align 1
  %r = call i79 @llvm.fshr.i79(i79 %1, i79 %1, i79 1)
  store i79 %r, ptr @a, align 1
  ret void
}
