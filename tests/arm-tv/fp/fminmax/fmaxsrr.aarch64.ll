; ModuleID = '<stdin>'
source_filename = "<stdin>"

declare void @dummy()

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare float @llvm.maximum.f32(float, float) #0

define float @f(i1 %0, float %1, float %2) {
  br i1 %0, label %4, label %5

4:                                                ; preds = %3
  call void @dummy()
  br label %5

5:                                                ; preds = %4, %3
  %6 = phi float [ %1, %3 ], [ %2, %4 ]
  %7 = phi float [ %2, %3 ], [ %1, %4 ]
  %8 = call float @llvm.maximum.f32(float %6, float %7)
  ret float %8
}


