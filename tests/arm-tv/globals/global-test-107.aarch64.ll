@g = external dso_local global i64

define i64 @f3() {
  %1 = load i64, ptr @g, align 4
  ret i64 %1
}