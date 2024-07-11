@a = external global i113

declare i113 @llvm.fshr.i113 (i113 %a, i113 %b, i113 %c)

define void @f() {
  %1 = load i113, ptr @a, align 1
  %r = call i113 @llvm.fshr.i113(i113 %1, i113 %1, i113 1)
  store i113 %r, ptr @a, align 1
  ret void
}
