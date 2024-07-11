define void @f() {
  %1 = alloca [100 x i8], align 1
  %2 = getelementptr inbounds [100 x i8], ptr %1, i32 0, i32 50
  %3 = getelementptr inbounds [100 x i8], ptr %1, i32 0, i32 -50
  call void @llvm.prefetch.p0(ptr %1, i32 0, i32 3, i32 0)
  call void @llvm.prefetch.p0(ptr %2, i32 0, i32 3, i32 0)
  call void @llvm.prefetch.p0(ptr %3, i32 0, i32 3, i32 0)
  ret void
}

; Function Attrs: nocallback nofree nosync nounwind willreturn memory(argmem: readwrite, inaccessiblemem: readwrite)
declare void @llvm.prefetch.p0(ptr nocapture readonly, i32 immarg, i32 immarg, i32 immarg) #0
