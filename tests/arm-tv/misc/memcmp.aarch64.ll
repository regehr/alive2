declare i32 @memcmp(ptr, ptr, i64)

define i1 @f(ptr %x, ptr %y) {
  %4 = tail call i32 @memcmp(ptr %x, ptr %y, i64 7)
  %5 = icmp eq i32 %4, 0
  ret i1 %5
}
