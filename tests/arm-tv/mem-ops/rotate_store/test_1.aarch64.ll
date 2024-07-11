@a = external global i1

declare i1 @llvm.fshr.i1 (i1 %a, i1 %b, i1 %c)

define void @f(i1 %arg) {
  %r = call i1 @llvm.fshr.i1(i1 %arg, i1 %arg, i1 1)
  store i1 %r, ptr @a, align 1
  ret void
}
