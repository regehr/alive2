; Function Attrs: strictfp
define i1 @_ZN11xalanc_1_1013DoubleSupport8lessThanEdd() #0 {
entry:
  %cmp4 = tail call i1 @llvm.experimental.constrained.fcmps.f64(double 0.000000e+00, double 1.000000e+00, metadata !"olt", metadata !"fpexcept.strict") #0
  ret i1 false
}

; Function Attrs: nocallback nofree nosync nounwind willreturn memory(inaccessiblemem: readwrite)
declare i1 @llvm.experimental.constrained.fcmps.f64(double, double, metadata, metadata) #1

attributes #0 = { strictfp }
attributes #1 = { nocallback nofree nosync nounwind willreturn memory(inaccessiblemem: readwrite) }