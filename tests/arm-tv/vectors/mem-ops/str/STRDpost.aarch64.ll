define <8 x i8> @f(<16 x i8> %0) {
  %2 = alloca i8, i32 8, align 16
  call void @llvm.masked.compressstore.v16i8(<16 x i8> %0, ptr %2, <16 x i1> <i1 true, i1 false, i1 true, i1 false, i1 true, i1 false, i1 true, i1 false, i1 true, i1 false, i1 true, i1 false, i1 true, i1 false, i1 true, i1 false>)
  %3 = load <8 x i8>, ptr %2, align 8
  ret <8 x i8> %3
}

; Function Attrs: nocallback nofree nosync nounwind willreturn memory(argmem: write)
declare void @llvm.masked.compressstore.v16i8(<16 x i8>, ptr nocapture, <16 x i1>) #0