; ModuleID = '<stdin>'
source_filename = "<stdin>"

define <4 x i32> @f(<4 x float> %0, <4 x float> %1, <4 x i32> %2, <4 x i32> %3) {
  %5 = fcmp olt <4 x float> %0, %1
  %6 = select <4 x i1> %5, <4 x i32> %2, <4 x i32> %3
  ret <4 x i32> %6
}
