target datalayout = "e-m:e-p:64:64-i64:64-i128:128-n32:64-S128"
target triple = "riscv64"

%struct.tf_0_struct_1 = type { i16, i64, i64, i16, i16 }

@tf_0_var_4 = external constant i16
@tf_0_struct_obj_3 = external global %struct.tf_0_struct_1

define i32 @tf_0_foo(ptr %tf_0_var_4, ptr %tf_0_struct_obj_3) {
entry:
  %0 = load i16, ptr %tf_0_var_4, align 2
  %conv = zext i16 %0 to i32
  %1 = load i16, ptr %tf_0_struct_obj_3, align 8
  %2 = or i16 %1, 1
  %sub3 = zext i16 %2 to i32
  %shl = shl i32 %conv, %sub3
  ret i32 %shl
}
