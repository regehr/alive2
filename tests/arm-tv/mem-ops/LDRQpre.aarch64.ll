define i32 @f(ptr nocapture readonly %0, ptr nocapture readonly %1, i32 %2) local_unnamed_addr {
  %4 = zext i32 %2 to i64
  br label %5

5:                                                ; preds = %5, %3
  %6 = phi i64 [ %18, %5 ], [ 0, %3 ]
  %7 = phi <32 x i32> [ %17, %5 ], [ zeroinitializer, %3 ]
  %8 = getelementptr inbounds i16, ptr %0, i64 %6
  %9 = bitcast ptr %8 to ptr
  %10 = load <32 x i16>, ptr %9, align 2
  %11 = zext <32 x i16> %10 to <32 x i32>
  %12 = getelementptr inbounds i16, ptr %1, i64 %6
  %13 = bitcast ptr %12 to ptr
  %14 = load <32 x i16>, ptr %13, align 2
  %15 = zext <32 x i16> %14 to <32 x i32>
  %16 = mul nsw <32 x i32> %15, %11
  %17 = add nsw <32 x i32> %16, %7
  %18 = add i64 %6, 16
  %19 = icmp eq i64 %18, %4
  br i1 %19, label %20, label %5

20:                                               ; preds = %5
  %21 = shufflevector <32 x i32> %17, <32 x i32> undef, <32 x i32> <i32 16, i32 17, i32 18, i32 19, i32 20, i32 21, i32 22, i32 23, i32 24, i32 25, i32 26, i32 27, i32 28, i32 29, i32 30, i32 31, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison>
  %22 = add <32 x i32> %17, %21
  %23 = shufflevector <32 x i32> %22, <32 x i32> undef, <32 x i32> <i32 8, i32 9, i32 10, i32 11, i32 12, i32 13, i32 14, i32 15, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison>
  %24 = add <32 x i32> %22, %23
  %25 = shufflevector <32 x i32> %24, <32 x i32> undef, <32 x i32> <i32 4, i32 5, i32 6, i32 7, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison>
  %26 = add <32 x i32> %24, %25
  %27 = shufflevector <32 x i32> %26, <32 x i32> undef, <32 x i32> <i32 2, i32 3, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison>
  %28 = add <32 x i32> %26, %27
  %29 = shufflevector <32 x i32> %28, <32 x i32> undef, <32 x i32> <i32 1, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison>
  %30 = add <32 x i32> %28, %29
  %31 = extractelement <32 x i32> %30, i32 0
  ret i32 %31
}