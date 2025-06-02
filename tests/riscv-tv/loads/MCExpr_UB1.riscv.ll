target datalayout = "e-m:e-p:64:64-i64:64-i128:128-n32:64-S128"
target triple = "riscv64"

%struct.tf_0_struct_1 = type { i64, i64, i8, i32, i32 }

@tf_0_array_1 = external global [8 x %struct.tf_0_struct_1]

define void @tf_0_foo(ptr %p) {
entry:
  %0 = load i64, ptr getelementptr inbounds nuw (i8, ptr @tf_0_array_1, i64 32), align 8
  %cmp = icmp ne i64 0, %0
  br i1 %cmp, label %if.then20, label %if.end51

if.then20:                                        ; preds = %entry
  store i8 0, ptr %p, align 1
  br label %if.end51

if.end51:                                         ; preds = %if.then20, %entry
  ret void
}
