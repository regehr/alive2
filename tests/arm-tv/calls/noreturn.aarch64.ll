; ModuleID = 'reduced.bc'
target datalayout = "e-m:o-i64:64-i128:128-n32:64-S128"
target triple = "arm64-apple-macosx14.0.0"

; Function Attrs: noreturn strictfp
declare void @_ZNSt3__120__throw_length_errorB8se180000EPKc() #0

; Function Attrs: strictfp
define void @_ZNKSt3__16vectorIbNS_9allocatorIbEEE20__throw_length_errorB8se180000Ev() #1 {
entry:
  call void @_ZNSt3__120__throw_length_errorB8se180000EPKc()
  unreachable
}

attributes #0 = { noreturn strictfp }
attributes #1 = { strictfp }
