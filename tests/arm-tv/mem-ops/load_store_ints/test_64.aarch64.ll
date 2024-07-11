@a = external global i64

declare i64 @llvm.fshr.i64 (i64 %a, i64 %b, i64 %c)

define void @f() {
  %1 = load i64, ptr @a, align 1
  %r = call i64 @llvm.fshr.i64(i64 %1, i64 %1, i64 1)
  store i64 %r, ptr @a, align 1
  ret void
}
