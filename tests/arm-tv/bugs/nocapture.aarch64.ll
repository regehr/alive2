; ModuleID = 'reduced.bc'
target datalayout = "e-m:o-i64:64-i128:128-n32:64-S128"
target triple = "arm64-apple-macosx14.0.0"

%a = type { [1 x i32], [1 x i64] }

define i64 @f(ptr nocapture %s, i64 %0) {
  %i = getelementptr %a, ptr %s, i64 0, i32 1, i64 %0
  %r = load i64, ptr %i, align 1
  ret i64 %r
}

