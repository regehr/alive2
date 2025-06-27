target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i8:8:32-i16:16:32-i64:64-i128:128-n32:64-S128-Fn32"
target triple = "aarch64-unknown-linux-gnu"

define i8 @tf_0_foo(i8 %0) {
entry:
  %1 = lshr i8 %0, 14
  %2 = and i8 %1, 1
  ret i8 %2
}
