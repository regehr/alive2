target datalayout = "e-m:e-p:64:64-i64:64-i128:128-n32:64-S128"
target triple = "riscv64"

define i32 @tf_0_foo(ptr %tf_0_array_6, ptr %0) {
entry:
  %bf.load55 = load i32, ptr %tf_0_array_6, align 8
  %1 = load i32, ptr %0, align 4
  ret i32 %1
}
