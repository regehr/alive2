; Function Attrs: nounwind ssp uwtable
define void @f() {
  %1 = alloca i32, align 4
  %2 = alloca i32, align 4
  %3 = alloca i32, align 4
  %4 = alloca i32, align 4
  store i32 0, ptr %1, align 4
  store i32 1, ptr %3, align 4
  store i32 1, ptr %2, align 4
  store i32 0, ptr %4, align 4
  %5 = load i32, ptr %2, align 4
  %6 = icmp ne i32 %5, 0
  br i1 %6, label %7, label %8

7:                                                ; preds = %0
  store i32 0, ptr %1, align 4
  br label %17

8:                                                ; preds = %0
  %9 = load i32, ptr %3, align 4
  %10 = icmp ne i32 %9, 0
  br i1 %10, label %11, label %16

11:                                               ; preds = %8
  %12 = load i32, ptr %4, align 4
  %13 = icmp ne i32 %12, 0
  br i1 %13, label %14, label %15

14:                                               ; preds = %11
  store i32 1, ptr %1, align 4
  br label %17

15:                                               ; preds = %11
  store i32 0, ptr %1, align 4
  br label %17

16:                                               ; preds = %8
  br label %17

17:                                               ; preds = %16, %15, %14, %7
  %18 = load i32, ptr %1, align 4
  ret void
}