@a = external global i45

declare i45 @llvm.fshr.i45 (i45 %a, i45 %b, i45 %c)

define void @f(i45 %arg) {
  %r = call i45 @llvm.fshr.i45(i45 %arg, i45 %arg, i45 1)
  store i45 %r, ptr @a, align 1
  ret void
}
