
@g = external global i32

define void @f1(i64 %0) {
  %2 = and i64 %0, 4294967296
  %3 = icmp eq i64 %2, 0
  br i1 %3, label %5, label %4

4:                                                ; preds = %1
  store i32 1, ptr @g, align 4
  br label %5

5:                                                ; preds = %4, %1
  ret void
}
