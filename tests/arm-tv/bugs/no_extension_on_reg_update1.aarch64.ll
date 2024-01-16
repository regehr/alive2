define i32 @f(ptr %0, ptr %1, i1 %2) {
  br label %4

4:                                                ; preds = %4, %3
  %5 = phi ptr [ %1, %3 ], [ %6, %4 ]
  %6 = getelementptr inbounds i8, ptr %5, i32 1
  %7 = load i8, ptr %5, align 1
  br i1 %2, label %4, label %8

8:                                                ; preds = %4
  %9 = zext i8 %7 to i32
  ret i32 %9
}