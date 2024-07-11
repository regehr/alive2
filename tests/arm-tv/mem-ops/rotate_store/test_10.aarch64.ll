@a = external global i10

declare i10 @llvm.fshr.i10 (i10 %a, i10 %b, i10 %c)

define void @f(i10 %arg) {
  %r = call i10 @llvm.fshr.i10(i10 %arg, i10 %arg, i10 1)
  store i10 %r, ptr @a, align 1
  ret void
}
