define void @f(double %0, double %1, i32 %2, i32 %3) {
  %5 = fcmp ugt double %0, %1
  %6 = icmp ugt i32 %2, %3
  %7 = and i1 %5, %6
  br i1 %7, label %8, label %9, !unpredictable !0

8:                                                ; preds = %4
  call void @f1()
  ret void

9:                                                ; preds = %4
  call void @f2()
  ret void
}

declare void @f1()

declare void @f2()



!0 = !{}
