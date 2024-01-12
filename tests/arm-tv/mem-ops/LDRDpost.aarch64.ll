define i64 @f(ptr %0, i32 %1) {
  %3 = zext i32 %1 to i64
  br label %4

4:                                                ; preds = %4, %2
  %5 = phi i64 [ %15, %4 ], [ 0, %2 ]
  %6 = phi <8 x i32> [ %14, %4 ], [ zeroinitializer, %2 ]
  %7 = phi <8 x i32> [ %12, %4 ], [ zeroinitializer, %2 ]
  %8 = getelementptr inbounds i8, ptr %0, i64 %5
  %9 = bitcast ptr %8 to ptr
  %10 = load <8 x i8>, ptr %9, align 1
  %11 = zext <8 x i8> %10 to <8 x i32>
  %12 = add nsw <8 x i32> %11, %7
  %13 = mul nsw <8 x i32> %11, %11
  %14 = add nsw <8 x i32> %13, %6
  %15 = add i64 %5, 8
  %16 = icmp eq i64 %15, %3
  br i1 %16, label %17, label %4

17:                                               ; preds = %4
  %18 = shufflevector <8 x i32> %12, <8 x i32> undef, <8 x i32> <i32 4, i32 5, i32 6, i32 7, i32 poison, i32 poison, i32 poison, i32 poison>
  %19 = add <8 x i32> %12, %18
  %20 = shufflevector <8 x i32> %19, <8 x i32> undef, <8 x i32> <i32 2, i32 3, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison>
  %21 = add <8 x i32> %19, %20
  %22 = shufflevector <8 x i32> %21, <8 x i32> undef, <8 x i32> <i32 1, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison>
  %23 = add <8 x i32> %21, %22
  %24 = extractelement <8 x i32> %23, i32 0
  %25 = shufflevector <8 x i32> %14, <8 x i32> undef, <8 x i32> <i32 4, i32 5, i32 6, i32 7, i32 poison, i32 poison, i32 poison, i32 poison>
  %26 = add <8 x i32> %14, %25
  %27 = shufflevector <8 x i32> %26, <8 x i32> undef, <8 x i32> <i32 2, i32 3, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison>
  %28 = add <8 x i32> %26, %27
  %29 = shufflevector <8 x i32> %28, <8 x i32> undef, <8 x i32> <i32 1, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison>
  %30 = add <8 x i32> %28, %29
  %31 = extractelement <8 x i32> %30, i32 0
  %32 = zext i32 %24 to i64
  %33 = shl nuw i64 %32, 32
  %34 = zext i32 %31 to i64
  %35 = or i64 %33, %34
  ret i64 %35
}