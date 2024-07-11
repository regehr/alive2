@a = external global i64

declare i64 @llvm.fshr.i64 (i64 %a, i64 %b, i64 %c)

define i64 @f() {
  %1 = load i64, ptr @a, align 1
  %r = call i64 @llvm.fshr.i64(i64 %1, i64 %1, i64 1)
  ret i64 %r
}
