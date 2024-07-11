; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare i8 @llvm.vector.reduce.smax.v2i8(<2 x i8>) #0

define i8 @f(ptr %0) {
  %2 = load <2 x i8>, ptr %0, align 2
  %3 = call i8 @llvm.vector.reduce.smax.v2i8(<2 x i8> %2)
  ret i8 %3
}