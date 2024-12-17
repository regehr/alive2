; ModuleID = '<stdin>'
source_filename = "<stdin>"

define double @fp-armv8_vmaxnm_NNNule_rev(double %0) {
  %2 = fcmp ule double 7.800000e+01, %0
  %3 = select i1 %2, double %0, double 7.800000e+01
  %4 = fcmp ule double %3, 9.000000e+01
  %5 = select i1 %4, double 9.000000e+01, double %3
  ret double %5
}
