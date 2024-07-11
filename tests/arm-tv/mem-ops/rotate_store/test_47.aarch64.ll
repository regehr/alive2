@a = external global i47

declare i47 @llvm.fshr.i47 (i47 %a, i47 %b, i47 %c)

define void @f(i47 %arg) {
  %r = call i47 @llvm.fshr.i47(i47 %arg, i47 %arg, i47 1)
  store i47 %r, ptr @a, align 1
  ret void
}
