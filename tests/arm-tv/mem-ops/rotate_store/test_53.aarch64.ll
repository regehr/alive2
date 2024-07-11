@a = external global i53

declare i53 @llvm.fshr.i53 (i53 %a, i53 %b, i53 %c)

define void @f(i53 %arg) {
  %r = call i53 @llvm.fshr.i53(i53 %arg, i53 %arg, i53 1)
  store i53 %r, ptr @a, align 1
  ret void
}
