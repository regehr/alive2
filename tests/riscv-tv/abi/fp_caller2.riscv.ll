; ModuleID = '<stdin>'
source_filename = "<stdin>"

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare float @g(i32, float, i32, i32, float) #0

define float @f(float nofpclass(inf nzero) %0, float %1, i32 %a, i32 %b, i32 %c) {
  %ret = call float @g(i32 %a, float %0, i32 %b, i32 %c, float %1)
  ret float %ret
}
