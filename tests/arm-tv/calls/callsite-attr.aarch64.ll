declare ptr @malloc(i64)

define i1 @f(ptr %0) {
  %2 = call nonnull ptr @malloc(i64 16)
  %3 = icmp eq ptr %2, %0
  ret i1 %3
}
