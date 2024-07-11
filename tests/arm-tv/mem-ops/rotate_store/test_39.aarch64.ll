@a = external global i39

declare i39 @llvm.fshr.i39 (i39 %a, i39 %b, i39 %c)

define void @f(i39 %arg) {
  %r = call i39 @llvm.fshr.i39(i39 %arg, i39 %arg, i39 1)
  store i39 %r, ptr @a, align 1
  ret void
}
