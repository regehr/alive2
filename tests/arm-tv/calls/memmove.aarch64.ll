; ModuleID = 'reduced.bc'
target datalayout = "e-m:o-i64:64-i128:128-n32:64-S128"
target triple = "arm64-apple-macosx14.0.0"

; Function Attrs: strictfp
define fastcc i8 @vect_slp_analyze_operations(ptr, ptr) #0 {
entry:
  tail call void @llvm.memmove.p0.p0.i64(ptr %0, ptr %1, i64 343560, i1 false) #0
  ret i8 0
}

; Function Attrs: nocallback nofree nounwind willreturn memory(argmem: readwrite)
declare void @llvm.memmove.p0.p0.i64(ptr nocapture writeonly, ptr nocapture readonly, i64, i1 immarg) #1

attributes #0 = { strictfp }
attributes #1 = { nocallback nofree nounwind willreturn memory(argmem: readwrite) }
