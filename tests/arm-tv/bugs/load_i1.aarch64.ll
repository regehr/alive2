; ModuleID = 'reduced.bc'
target datalayout = "e-m:o-i64:64-i128:128-n32:64-S128"
target triple = "arm64-apple-macosx14.0.0"

; Function Attrs: strictfp
define i32 @errors(ptr %errs) #0 {
entry:
  %.b = load i1, ptr %errs, align 4
  %0 = zext i1 %.b to i32
  ret i32 %0
}

attributes #0 = { strictfp }
