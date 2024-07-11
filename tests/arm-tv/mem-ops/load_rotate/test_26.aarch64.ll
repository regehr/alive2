@a = external global i26

declare i26 @llvm.fshr.i26 (i26 %a, i26 %b, i26 %c)

define i26 @f() {
  %1 = load i26, ptr @a, align 1
  %r = call i26 @llvm.fshr.i26(i26 %1, i26 %1, i26 1)
  ret i26 %r
}
