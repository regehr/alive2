; ModuleID = 'f.c'
source_filename = "f.c"
target datalayout = "e-m:e-p:64:64-i64:64-i128:128-n32:64-S128"
target triple = "riscv64"

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(none)
define dso_local signext i32 @f(i32 noundef signext %a) local_unnamed_addr #0 {
entry:
  %conv = sitofp i32 %a to float
  %add1 = fadd float %conv, 6.000000e+00
  %conv2 = fptosi float %add1 to i32
  ret i32 %conv2
}
