@a = external global i55

declare i55 @llvm.fshr.i55 (i55 %a, i55 %b, i55 %c)

define void @f(i55 %arg) {
  %r = call i55 @llvm.fshr.i55(i55 %arg, i55 %arg, i55 1)
  store i55 %r, ptr @a, align 1
  ret void
}
