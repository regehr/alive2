; Function Attrs: strictfp
define double @_ZN11xalanc_1_1013DoubleSupport8negativeEd(double %theDouble) #0 {
entry:
  %fneg = fneg double %theDouble
  ret double %fneg
}

attributes #0 = { strictfp }