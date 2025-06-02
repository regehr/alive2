target datalayout = "e-m:e-p:64:64-i64:64-i128:128-n32:64-S128"
target triple = "riscv64"

%struct.tf_0_struct_5 = type { %struct.tf_0_struct_1, i64, %struct.tf_0_struct_4, i64, i64, i64 }
%struct.tf_0_struct_1 = type { i64, i64, i64, i64, i64, i64, i64, i64 }
%struct.tf_0_struct_4 = type { %struct.tf_0_struct_2, i64, %struct.tf_0_struct_3, i64, %struct.tf_0_struct_3, i64, %struct.tf_0_struct_1, i64, i64, %struct.tf_0_struct_1 }
%struct.tf_0_struct_2 = type { %struct.tf_0_struct_1, %struct.tf_0_struct_1, i64, %struct.tf_0_struct_1, i64, %struct.tf_0_struct_1, i64, %struct.tf_0_struct_1, %struct.tf_0_struct_1 }
%struct.tf_0_struct_3 = type { i64, i64, i64, i64 }

@tf_0_array_1 = external global [10 x %struct.tf_0_struct_5]

define i64 @tf_0_foo() {
entry:
  %0 = load i64, ptr getelementptr inbounds nuw (i8, ptr @tf_0_array_1, i64 2928), align 8
  ret i64 %0
}
