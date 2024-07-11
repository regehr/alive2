@a = external global i67

declare i67 @llvm.fshr.i67 (i67 %a, i67 %b, i67 %c)

define void @f() {
  %1 = load i67, ptr @a, align 1
  %r = call i67 @llvm.fshr.i67(i67 %1, i67 %1, i67 1)
  store i67 %r, ptr @a, align 1
  ret void
}
