target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i8:8:32-i16:16:32-i64:64-i128:128-n32:64-S128-Fn32"
target triple = "aarch64-unknown-linux-gnu"

define ptr @test(i32 %conv48) {
entry:
  %0 = zext i32 %conv48 to i64
  %arrayidx = getelementptr [13 x i32], ptr null, i64 0, i64 %0
  ret ptr %arrayidx
}
