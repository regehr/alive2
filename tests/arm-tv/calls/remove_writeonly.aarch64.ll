; ModuleID = 'reduced.bc'
target datalayout = "e-m:o-i64:64-i128:128-n32:64-S128"
target triple = "arm64-apple-macosx14.0.0"

define ptr @src(ptr returned %this) {
entry:
  store ptr null, ptr %this, align 8
  ret ptr %this
}


define noundef ptr @tgt(ptr returned writeonly %0) {
arm_tv_entry:
  store i64 0, ptr %0, align 1
  ret ptr %0
}
