define i32 @f(ptr %0, i1 %1) {
  br label %3

3:                                                ; preds = %3, %2
  %4 = phi i64 [ 0, %2 ], [ %8, %3 ]
  %5 = getelementptr inbounds i32, ptr %0, i64 %4
  %6 = load i32, ptr %5, align 4
  %7 = icmp eq i32 %6, 0
  %8 = add i64 %4, 1
  br i1 %1, label %9, label %3

9:                                                ; preds = %3
  %10 = zext i1 %7 to i32
  ret i32 0
}