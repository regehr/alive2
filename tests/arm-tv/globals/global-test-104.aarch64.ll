
@g = external global i32

define void @f13(i32 %0) {
  %2 = and i32 %0, 140
  %3 = icmp ugt i32 %2, 126
  br i1 %3, label %5, label %4

4:                                                ; preds = %1
  store i32 1, ptr @g, align 4
  br label %5

5:                                                ; preds = %4, %1
  ret void
}
