; Function Attrs: nocallback nofree nounwind willreturn memory(argmem: readwrite)
declare void @llvm.memcpy.inline.p0.p0.i32(ptr noalias nocapture writeonly, ptr noalias nocapture readonly, i32 immarg, i1 immarg) #0

; Function Attrs: nounwind
define void @f(ptr %0, ptr %1) {
  %3 = alloca [16 x i8], align 1
  call void @llvm.memcpy.inline.p0.p0.i32(ptr align 1 %3, ptr align 1 %1, i32 16, i1 false)
  call void @llvm.memcpy.inline.p0.p0.i32(ptr align 1 %1, ptr align 1 %3, i32 16, i1 false)
  ret void
}