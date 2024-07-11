@a = external global i48

declare i48 @llvm.fshr.i48 (i48 %a, i48 %b, i48 %c)

define i48 @f() {
  %1 = load i48, ptr @a, align 1
  %r = call i48 @llvm.fshr.i48(i48 %1, i48 %1, i48 1)
  ret i48 %r
}
