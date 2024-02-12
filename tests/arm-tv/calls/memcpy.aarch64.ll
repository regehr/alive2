; ModuleID = 'reduced.bc'
target datalayout = "e-m:o-i64:64-i128:128-n32:64-S128"
target triple = "arm64-apple-macosx14.0.0"

; Function Attrs: strictfp
define void @_ZN20ComputeNonbondedUtil26calc_pair_energy_fullelectEP9nonbonded(i64 %conv23.i.i.i) #0 {
entry:
  tail call void @llvm.memcpy.p0.p0.i64(ptr null, ptr null, i64 %conv23.i.i.i, i1 false) #0
  ret void
}

; Function Attrs: nocallback nofree nounwind willreturn memory(argmem: readwrite)
declare void @llvm.memcpy.p0.p0.i64(ptr noalias nocapture writeonly, ptr noalias nocapture readonly, i64, i1 immarg) #1

attributes #0 = { strictfp }
attributes #1 = { nocallback nofree nounwind willreturn memory(argmem: readwrite) }
