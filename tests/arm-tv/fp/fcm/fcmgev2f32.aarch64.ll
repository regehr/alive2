; ModuleID = '<stdin>'
source_filename = "<stdin>"

define <2 x float> @f(<2 x float> %0, <2 x float> %1) {
  %3 = fcmp oge <2 x float> %0, %1
  %4 = select <2 x i1> %3, <2 x float> %0, <2 x float> %1
  ret <2 x float> %4
}
