; Function Attrs: nounwind
define void @f(ptr %0) {
  call void @llvm.prefetch.p0(ptr %0, i32 0, i32 2, i32 1)
  ret void
}

; Function Attrs: nocallback nofree nosync nounwind willreturn memory(argmem: readwrite, inaccessiblemem: readwrite)
declare void @llvm.prefetch.p0(ptr nocapture readonly, i32 immarg, i32 immarg, i32 immarg) #1