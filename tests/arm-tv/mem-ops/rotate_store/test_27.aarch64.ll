@a = external global i27

declare i27 @llvm.fshr.i27 (i27 %a, i27 %b, i27 %c)

define void @f(i27 %arg) {
  %r = call i27 @llvm.fshr.i27(i27 %arg, i27 %arg, i27 1)
  store i27 %r, ptr @a, align 1
  ret void
}
