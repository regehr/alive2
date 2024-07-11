; ModuleID = 'reduced.bc'
target datalayout = "e-m:o-i64:64-i128:128-n32:64-S128"
target triple = "arm64-apple-macosx14.0.0"

; Function Attrs: willreturn
declare i64 @strlen() #0

; Function Attrs: strictfp
define i32 @DynamicPaintSurface_output_name_b_length() #1 {
entry:
  %call = call i64 @strlen()
  ret i32 0
}

attributes #0 = { willreturn }
attributes #1 = { strictfp }
