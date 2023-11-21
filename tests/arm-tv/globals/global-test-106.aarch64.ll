@dsolocalvar = external dso_local local_unnamed_addr global i32, align 4

define dso_local i32 @getDsoLocalVar() {
  %1 = load i32, ptr @dsolocalvar, align 4
  ret i32 %1
}