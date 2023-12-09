; Function Attrs: sspstrong
define i64 @bitcast_larger_load() {
  %1 = alloca i32, align 4
  store i32 0, ptr %1, align 4
  %2 = load i64, ptr %1, align 4
  ret i64 %2
}