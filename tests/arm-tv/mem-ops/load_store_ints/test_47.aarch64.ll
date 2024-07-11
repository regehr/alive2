@a = external global i47

declare i47 @llvm.fshr.i47 (i47 %a, i47 %b, i47 %c)

define void @f() {
  %1 = load i47, ptr @a, align 1
  %r = call i47 @llvm.fshr.i47(i47 %1, i47 %1, i47 1)
  store i47 %r, ptr @a, align 1
  ret void
}
