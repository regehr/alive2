@a = external global i103

declare i103 @llvm.fshr.i103 (i103 %a, i103 %b, i103 %c)

define void @f() {
  %1 = load i103, ptr @a, align 1
  %r = call i103 @llvm.fshr.i103(i103 %1, i103 %1, i103 1)
  store i103 %r, ptr @a, align 1
  ret void
}
