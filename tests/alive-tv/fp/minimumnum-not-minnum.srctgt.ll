; ERROR: Value mismatch
; CHECK: 4 incorrect transformations

define float @src_minnum32(float noundef %a, float noundef %b) {
  %r = call float @llvm.minimumnum.f32(float %a, float %b)
  ret float %r
}
define float @tgt_minnum32(float noundef %a, float noundef %b) {
  %r = call float @llvm.minnum.f32(float %a, float %b)
  ret float %r
}

define double @src_minnum64(double noundef %a, double noundef %b) {
  %r = call double @llvm.minimumnum.f64(double %a, double %b)
  ret double %r
}
define double @tgt_minnum64(double noundef %a, double noundef %b) {
  %r = call double @llvm.minnum.f64(double %a, double %b)
  ret double %r
}

define float @src_maxnum32(float noundef %a, float noundef %b) {
  %r = call float @llvm.maximumnum.f32(float %a, float %b)
  ret float %r
}
define float @tgt_maxnum32(float noundef %a, float noundef %b) {
  %r = call float @llvm.maxnum.f32(float %a, float %b)
  ret float %r
}

define double @src_maxnum64(double noundef %a, double noundef %b) {
  %r = call double @llvm.maximumnum.f64(double %a, double %b)
  ret double %r
}
define double @tgt_maxnum64(double noundef %a, double noundef %b) {
  %r = call double @llvm.maxnum.f64(double %a, double %b)
  ret double %r
}
