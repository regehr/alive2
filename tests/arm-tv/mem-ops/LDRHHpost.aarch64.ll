define void @f(ptr nocapture readonly %0, ptr nocapture %1, i32 %2) {
  %4 = icmp eq i32 %2, 0
  br i1 %4, label %21, label %5

5:                                                ; preds = %3
  br label %6

6:                                                ; preds = %6, %5
  %7 = phi ptr [ %10, %6 ], [ %0, %5 ]
  %8 = phi i32 [ %18, %6 ], [ %2, %5 ]
  %9 = phi ptr [ %17, %6 ], [ %1, %5 ]
  %10 = getelementptr inbounds i16, ptr %7, i32 1
  %11 = load i16, ptr %7, align 2
  %12 = icmp sgt i16 %11, 0
  %13 = icmp eq i16 %11, -32768
  %14 = sub i16 0, %11
  %15 = select i1 %13, i16 32767, i16 %14
  %16 = select i1 %12, i16 %11, i16 %15
  %17 = getelementptr inbounds i16, ptr %9, i32 1
  store i16 %16, ptr %9, align 2
  %18 = add i32 %8, -1
  %19 = icmp eq i32 %18, 0
  br i1 %19, label %20, label %6

20:                                               ; preds = %6
  br label %21

21:                                               ; preds = %20, %3
  ret void
}