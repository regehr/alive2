declare ghccc i32 @addtwo(i32, i32)

declare ghccc void @foo()

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare i32 @llvm.ssub.sat.i32(i32, i32) #0

define void @f(i32 %0, i32 %1, i32 %2) {
  %4 = call i32 @llvm.ssub.sat.i32(i32 %1, i32 %1)
  %5 = call ghccc i32 @addtwo(i32 645153373, i32 %1)
  call void @foo()
  ret void
}

attributes #0 = { nocallback nofree nosync nounwind speculatable willreturn memory(none) }
