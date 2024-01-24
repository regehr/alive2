; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare i16 @llvm.vector.reduce.add.v32i16(<32 x i16>) #0

define i16 @f(ptr %0) {
  %2 = load <32 x i8>, ptr %0, align 32
  %3 = sext <32 x i8> %2 to <32 x i16>
  %4 = call i16 @llvm.vector.reduce.add.v32i16(<32 x i16> %3)
  ret i16 %4
}