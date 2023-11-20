@ch8 = external dso_local local_unnamed_addr global i8, align 4

define i64 @zextload8() {
  %1 = load i8, ptr @ch8, align 4
  %2 = zext i8 %1 to i64
  ret i64 %2
}