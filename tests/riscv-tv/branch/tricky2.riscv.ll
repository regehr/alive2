define void @tf_0_foo(ptr %p) {
entry:
  %tobool229.not = icmp eq i64 0, 1
  br i1 %tobool229.not, label %lor.lhs.false230, label %if.then235

lor.lhs.false230:                                 ; preds = %entry
  %tobool234.not = icmp eq i16 0, 0
  br i1 %tobool234.not, label %if.end310, label %if.then235

if.then235:                                       ; preds = %lor.lhs.false230, %entry
  store i16 0, ptr %p, align 8
  br label %if.end310

if.end310:                                        ; preds = %if.then235, %lor.lhs.false230
  ret void
}
