define ptr @f(ptr readonly %0) {
  %2 = getelementptr inbounds i8, ptr %0, i64 -1
  %3 = load i8, ptr %2, align 1
  %4 = icmp sgt i8 %3, -1
  br i1 %4, label %9, label %5

5:                                                ; preds = %1
  %6 = getelementptr inbounds i8, ptr %0, i64 -2
  %7 = load i8, ptr %6, align 1
  %8 = icmp sgt i8 %7, -1
  br i1 %8, label %9, label %11

9:                                                ; preds = %5, %1
  %10 = phi ptr [ %2, %1 ], [ %6, %5 ]
  br label %11

11:                                               ; preds = %9, %5
  %12 = phi ptr [ %10, %9 ], [ null, %5 ]
  ret ptr %12
}