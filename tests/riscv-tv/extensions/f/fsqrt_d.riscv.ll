define dso_local noundef double @sqrt(double noundef %x) local_unnamed_addr #0 {
entry:
  %1 = call double @llvm.sqrt.f64(double %x)
  ret double %1
}
