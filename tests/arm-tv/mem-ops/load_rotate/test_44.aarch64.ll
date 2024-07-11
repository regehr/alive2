@a = external global i44

declare i44 @llvm.fshr.i44 (i44 %a, i44 %b, i44 %c)

define i44 @f() {
  %1 = load i44, ptr @a, align 1
  %r = call i44 @llvm.fshr.i44(i44 %1, i44 %1, i44 1)
  ret i44 %r
}
