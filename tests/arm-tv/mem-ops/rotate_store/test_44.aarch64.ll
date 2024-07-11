@a = external global i44

declare i44 @llvm.fshr.i44 (i44 %a, i44 %b, i44 %c)

define void @f(i44 %arg) {
  %r = call i44 @llvm.fshr.i44(i44 %arg, i44 %arg, i44 1)
  store i44 %r, ptr @a, align 1
  ret void
}
