; ModuleID = '<stdin>'
source_filename = "<stdin>"

define <4 x i32> @f(<4 x float> %0, <4 x float> %1) {
  %3 = fcmp nnan ole <4 x float> %0, %1
  %4 = sext <4 x i1> %3 to <4 x i32>
  ret <4 x i32> %4
}
