; Function Attrs: noinline nounwind optnone
define dso_local i32 @f(ptr %0, ptr %1) {
  %3 = alloca ptr, align 4
  %4 = alloca ptr, align 4
  store ptr %0, ptr %3, align 4
  store ptr %1, ptr %4, align 4
  %5 = load ptr, ptr %3, align 4
  %6 = load i32, ptr %5, align 4
  %7 = load ptr, ptr %4, align 4
  %8 = load i32, ptr %7, align 4
  %9 = add nsw i32 %6, %8
  ret i32 %9
}