target datalayout = "e-m:e-p:64:64-i64:64-i128:128-n32:64-S128"
target triple = "riscv64"

define void @tf_0_foo(i64 %conv63, ptr %p) {
entry:
  %cmp64.not = icmp slt i64 0, %conv63
  br i1 %cmp64.not, label %if.end125, label %if.then66

if.then66:                                        ; preds = %entry
  store i32 0, ptr %p, align 4
  br label %if.end125

if.end125:                                        ; preds = %if.then66, %entry
  ret void
}
