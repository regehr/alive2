@a = external global i45

declare i45 @llvm.fshr.i45 (i45 %a, i45 %b, i45 %c)

define i45 @f() {
  %1 = load i45, ptr @a, align 1
  %r = call i45 @llvm.fshr.i45(i45 %1, i45 %1, i45 1)
  ret i45 %r
}
