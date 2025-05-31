target datalayout = "e-m:e-p:64:64-i64:64-i128:128-n32:64-S128"
target triple = "riscv64"

%struct.tf_0_struct_3 = type { i32, i32, %struct.tf_0_struct_2, i32, i32, %struct.tf_0_struct_2 }
%struct.tf_0_struct_2 = type { %struct.tf_0_struct_1 }
%struct.tf_0_struct_1 = type { i32, i32, i32, i32, i32, i32 }

@tf_0_struct_obj_1 = external global %struct.tf_0_struct_3

define i32 @tf_0_foo() {
entry:
  %0 = load i32, ptr getelementptr inbounds nuw (i8, ptr @tf_0_struct_obj_1, i64 20), align 4
  ret i32 %0
}
