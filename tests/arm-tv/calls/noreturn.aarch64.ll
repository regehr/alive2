; ModuleID = 'reduced.bc'
target datalayout = "e-m:o-i64:64-i128:128-n32:64-S128"
target triple = "arm64-apple-macosx14.0.0"

define void @__clang_call_terminate() {
  tail call void @_ZSt9terminatev() #0
  unreachable
}

declare void @_ZSt9terminatev()

attributes #0 = { noreturn }
