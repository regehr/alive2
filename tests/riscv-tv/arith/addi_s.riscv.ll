target datalayout = "e-m:e-p:64:64-i64:64-i128:128-n32:64-S128"
target triple = "riscv64"

%struct.tf_0_struct_1 = type { i32, i32, i32, i8 }

@tf_0_array_3 = external global [8 x %struct.tf_0_struct_1]

define i1 @tf_0_foo() {
entry:
  %0 = load i32, ptr getelementptr inbounds nuw (i8, ptr @tf_0_array_3, i64 72), align 4
  %tobool3.not = icmp eq i32 %0, 0
  %1 = load i32, ptr @tf_0_array_3, align 4
  %tobool4.not53 = icmp eq i32 %1, 0
  %tobool4.not = select i1 %tobool3.not, i1 %tobool4.not53, i1 false
  ret i1 %tobool4.not
}
