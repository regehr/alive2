@a = external global i53

declare i53 @llvm.fshr.i53 (i53 %a, i53 %b, i53 %c)

define i53 @f() {
  %1 = load i53, ptr @a, align 1
  %r = call i53 @llvm.fshr.i53(i53 %1, i53 %1, i53 1)
  ret i53 %r
}
