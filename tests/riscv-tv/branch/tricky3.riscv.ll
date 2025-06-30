define i8 @tf_0_foo(ptr %p) {
entry:
  store i8 0, ptr %p, align 1
  %tobool59.not = icmp eq i32 0, 0
  br i1 %tobool59.not, label %if.end206, label %if.then

if.then:                                          ; preds = %entry
  %0 = load i8, ptr %p, align 1
  ret i8 %0

if.end206:                                        ; preds = %entry
  ret i8 0
}
