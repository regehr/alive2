; ModuleID = '<stdin>'
source_filename = "<stdin>"

define <2 x float> @f(<2 x float> %0) {
  %2 = fcmp oeq <2 x float> %0, splat (float 2.000000e+00)
  %3 = insertelement <2 x float> %0, float 4.000000e+00, i64 0
  %4 = select <2 x i1> %2, <2 x float> %3, <2 x float> <float 4.000000e+00, float 2.000000e+00>
  ret <2 x float> %4
}
