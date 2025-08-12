target datalayout = "e-m:e-p:64:64-i64:64-i128:128-n32:64-S128"
target triple = "riscv64"

; Function Attrs: minsize
define i64 @tf_0_foo(i1 %tobool34.not) #0 {
entry:
  br i1 %tobool34.not, label %cond.false, label %cond.true

cond.true:                                        ; preds = %entry
  %.pre = load i64, ptr null, align 8
  %.pre128 = load i8, ptr getelementptr inbounds nuw (i8, ptr null, i64 61), align 1
  %.pre129 = xor i8 %.pre128, -1
  %.pre130 = sext i8 %.pre129 to i64
  %.pre131 = or i64 %.pre, %.pre130
  %.pre132 = srem i64 %.pre131, 4294967295
  ret i64 %.pre132

cond.false:                                       ; preds = %entry
  %0 = load i64, ptr null, align 8
  %1 = load i8, ptr getelementptr inbounds nuw (i8, ptr null, i64 61), align 1
  %2 = xor i8 %1, -1
  %conv50 = sext i8 %2 to i64
  %or51 = or i64 %0, %conv50
  %rem = srem i64 %or51, 4294967295
  %tobool54.not = icmp eq i64 %rem, 0
  br i1 %tobool54.not, label %if.else, label %if.then55

if.then55:                                        ; preds = %cond.false
  ret i64 0

if.else:                                          ; preds = %cond.false
  ret i64 0
}

attributes #0 = { minsize }

; CHECK-NOT: OUTLINE
