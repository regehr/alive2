; Function Attrs: nocallback nofree nounwind willreturn memory(argmem: readwrite)
declare void @llvm.memcpy.p0.p0.i64(ptr noalias nocapture writeonly, ptr noalias nocapture readonly, i64, i1 immarg) #0

define zeroext i8 @f(ptr %0) {
  %2 = alloca [16 x i8], align 1
  %3 = alloca [16 x i8], align 1
  store i8 0, ptr %2, align 1
  call void @llvm.memcpy.p0.p0.i64(ptr %3, ptr %2, i64 16, i1 false)
  %4 = load i8, ptr %3, align 1
  ret i8 %4
}

attributes #0 = { nocallback nofree nounwind willreturn memory(argmem: readwrite) }