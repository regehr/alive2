; With one sNaN and one number, minnum/maxnum may return NaN.
; minimumnum/maximumnum must return the number.
; CHECK: 4 correct transformations
; CHECK: 0 incorrect transformations
; CHECK: 0 failed-to-prove transformations

define float @src_minnum_f32(float noundef %a, float noundef %b) {
  %r = call float @llvm.minnum.f32(float %a, float %b)
  ret float %r
}
define float @tgt_minnum_f32(float noundef %a, float noundef %b) {
  %r = call float @llvm.minimumnum.f32(float %a, float %b)
  ret float %r
}

define double @src_minnum_f64(double noundef %a, double noundef %b) {
  %r = call double @llvm.minnum.f64(double %a, double %b)
  ret double %r
}
define double @tgt_minnum_f64(double noundef %a, double noundef %b) {
  %r = call double @llvm.minimumnum.f64(double %a, double %b)
  ret double %r
}

define float @src_maxnum_f32(float noundef %a, float noundef %b) {
  %r = call float @llvm.maxnum.f32(float %a, float %b)
  ret float %r
}
define float @tgt_maxnum_f32(float noundef %a, float noundef %b) {
  %r = call float @llvm.maximumnum.f32(float %a, float %b)
  ret float %r
}

define double @src_maxnum_f64(double noundef %a, double noundef %b) {
  %r = call double @llvm.maxnum.f64(double %a, double %b)
  ret double %r
}
define double @tgt_maxnum_f64(double noundef %a, double noundef %b) {
  %r = call double @llvm.maximumnum.f64(double %a, double %b)
  ret double %r
}

declare double @llvm.maximumnum.f64(double, double)
declare double @llvm.maxnum.f64(double, double)
declare double @llvm.minimumnum.f64(double, double)
declare double @llvm.minnum.f64(double, double)
declare float @llvm.maximumnum.f32(float, float)
declare float @llvm.maxnum.f32(float, float)
declare float @llvm.minimumnum.f32(float, float)
declare float @llvm.minnum.f32(float, float)
