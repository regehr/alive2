define dso_local i64 @f(i32 %0, ptr nocapture readnone %1, ptr nocapture readonly %2, ptr nocapture readonly %3) {
  %5 = icmp sgt i32 %0, 0
  br i1 %5, label %6, label %9

6:                                                ; preds = %4
  %7 = load i8, ptr %3, align 2
  %8 = load i8, ptr %2, align 2
  br label %11

9:                                                ; preds = %11, %4
  %10 = phi i64 [ 0, %4 ], [ %30, %11 ]
  ret i64 %10

11:                                               ; preds = %11, %6
  %12 = phi i64 [ %30, %11 ], [ 0, %6 ]
  %13 = phi i32 [ %16, %11 ], [ 0, %6 ]
  %14 = getelementptr inbounds i8, ptr %3, i32 %13
  %15 = load i8, ptr %14, align 2
  %16 = add nuw nsw i32 %13, 1
  %17 = getelementptr inbounds i8, ptr %3, i32 %16
  %18 = load i8, ptr %17, align 2
  %19 = getelementptr inbounds i8, ptr %2, i32 %13
  %20 = load i8, ptr %19, align 2
  %21 = sext i8 %20 to i64
  %22 = sext i8 %15 to i64
  %23 = mul nsw i64 %21, %22
  %24 = getelementptr inbounds i8, ptr %2, i32 %16
  %25 = load i8, ptr %24, align 2
  %26 = sext i8 %25 to i64
  %27 = sext i8 %18 to i64
  %28 = mul nsw i64 %26, %27
  %29 = add i64 %23, %12
  %30 = add i64 %29, %28
  %31 = icmp ne i32 %16, %0
  br i1 %31, label %11, label %9
}