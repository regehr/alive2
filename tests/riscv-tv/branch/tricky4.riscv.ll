define i64 @tf_0_foo() {
entry:
  %tobool1413.not = icmp eq i32 0, 0
  br i1 %tobool1413.not, label %cond.true1575, label %cond.end1589

cond.true1575:                                    ; preds = %entry
  br label %cond.end1589

cond.end1589:                                     ; preds = %cond.true1575, %entry
  %cond1590 = phi i64 [ 0, %cond.true1575 ], [ 1, %entry ]
  ret i64 %cond1590
}
