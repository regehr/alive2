define i16 @f(i16 %0, i16 %1, i16 %2) {
  %4 = bitcast i16 %0 to <2 x i8>
  %5 = bitcast i16 %1 to <2 x i8>
  %6 = bitcast i16 %2 to <2 x i8>
  %7 = call <2 x i8> @llvm.fshr.v2i8(<2 x i8> %4, <2 x i8> %5, <2 x i8> %6)
  %8 = bitcast <2 x i8> %7 to i16
  ret i16 %8
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare <2 x i8> @llvm.fshr.v2i8(<2 x i8>, <2 x i8>, <2 x i8>) #0