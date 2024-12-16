; ModuleID = '<stdin>'
source_filename = "<stdin>"

; Function Attrs: nounwind
define i1 @f(float %0, float %1, float %2) {
  %4 = fcmp oge float %0, %2
  %5 = fcmp oge float %1, %2
  %6 = or i1 %4, %5
  ret i1 %6
}


