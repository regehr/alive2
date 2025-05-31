target datalayout = "e-m:e-p:64:64-i64:64-i128:128-n32:64-S128"
target triple = "riscv64"

define void @tf_0_foo(ptr %p) {
entry:
  store i32 -680676046, ptr %p, align 4
  ret void
}
