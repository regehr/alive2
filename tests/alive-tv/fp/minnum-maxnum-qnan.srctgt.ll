; ERROR: Value mismatch
; CHECK: 8 incorrect transformations
; A qNaN with a number must produce the number.

define float @src_minnum320() {
  %n = bitcast i32 2143289345 to float
  %r = call float @llvm.minnum.f32(float %n, float 1.0)
  ret float %r
}
define float @tgt_minnum320() {
  %n = bitcast i32 2143289345 to float
  ret float %n
}

define float @src_minnum321() {
  %n = bitcast i32 2143289345 to float
  %r = call float @llvm.minnum.f32(float 1.0, float %n)
  ret float %r
}
define float @tgt_minnum321() {
  %n = bitcast i32 2143289345 to float
  ret float %n
}

define double @src_minnum640() {
  %n = bitcast i64 9221120237041090561 to double
  %r = call double @llvm.minnum.f64(double %n, double 1.0)
  ret double %r
}
define double @tgt_minnum640() {
  %n = bitcast i64 9221120237041090561 to double
  ret double %n
}

define double @src_minnum641() {
  %n = bitcast i64 9221120237041090561 to double
  %r = call double @llvm.minnum.f64(double 1.0, double %n)
  ret double %r
}
define double @tgt_minnum641() {
  %n = bitcast i64 9221120237041090561 to double
  ret double %n
}

define float @src_maxnum320() {
  %n = bitcast i32 2143289345 to float
  %r = call float @llvm.maxnum.f32(float %n, float 1.0)
  ret float %r
}
define float @tgt_maxnum320() {
  %n = bitcast i32 2143289345 to float
  ret float %n
}

define float @src_maxnum321() {
  %n = bitcast i32 2143289345 to float
  %r = call float @llvm.maxnum.f32(float 1.0, float %n)
  ret float %r
}
define float @tgt_maxnum321() {
  %n = bitcast i32 2143289345 to float
  ret float %n
}

define double @src_maxnum640() {
  %n = bitcast i64 9221120237041090561 to double
  %r = call double @llvm.maxnum.f64(double %n, double 1.0)
  ret double %r
}
define double @tgt_maxnum640() {
  %n = bitcast i64 9221120237041090561 to double
  ret double %n
}

define double @src_maxnum641() {
  %n = bitcast i64 9221120237041090561 to double
  %r = call double @llvm.maxnum.f64(double 1.0, double %n)
  ret double %r
}
define double @tgt_maxnum641() {
  %n = bitcast i64 9221120237041090561 to double
  ret double %n
}
