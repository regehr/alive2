; CHECK: 16 correct transformations
; CHECK: 0 incorrect transformations
; An sNaN with a number may produce either the number or a NaN.

define float @src_minnum3200() {
  %n = bitcast i32 2139095041 to float
  %r = call float @llvm.minnum.f32(float %n, float 1.0)
  ret float %r
}
define float @tgt_minnum3200() {
  ret float 1.0
}

define float @src_minnum3201() {
  %n = bitcast i32 2139095041 to float
  %r = call float @llvm.minnum.f32(float %n, float 1.0)
  ret float %r
}
define float @tgt_minnum3201() {
  %n = bitcast i32 2143289345 to float
  ret float %n
}

define float @src_minnum3210() {
  %n = bitcast i32 2139095041 to float
  %r = call float @llvm.minnum.f32(float 1.0, float %n)
  ret float %r
}
define float @tgt_minnum3210() {
  ret float 1.0
}

define float @src_minnum3211() {
  %n = bitcast i32 2139095041 to float
  %r = call float @llvm.minnum.f32(float 1.0, float %n)
  ret float %r
}
define float @tgt_minnum3211() {
  %n = bitcast i32 2143289345 to float
  ret float %n
}

define double @src_minnum6400() {
  %n = bitcast i64 9218868437227405313 to double
  %r = call double @llvm.minnum.f64(double %n, double 1.0)
  ret double %r
}
define double @tgt_minnum6400() {
  ret double 1.0
}

define double @src_minnum6401() {
  %n = bitcast i64 9218868437227405313 to double
  %r = call double @llvm.minnum.f64(double %n, double 1.0)
  ret double %r
}
define double @tgt_minnum6401() {
  %n = bitcast i64 9221120237041090561 to double
  ret double %n
}

define double @src_minnum6410() {
  %n = bitcast i64 9218868437227405313 to double
  %r = call double @llvm.minnum.f64(double 1.0, double %n)
  ret double %r
}
define double @tgt_minnum6410() {
  ret double 1.0
}

define double @src_minnum6411() {
  %n = bitcast i64 9218868437227405313 to double
  %r = call double @llvm.minnum.f64(double 1.0, double %n)
  ret double %r
}
define double @tgt_minnum6411() {
  %n = bitcast i64 9221120237041090561 to double
  ret double %n
}

define float @src_maxnum3200() {
  %n = bitcast i32 2139095041 to float
  %r = call float @llvm.maxnum.f32(float %n, float 1.0)
  ret float %r
}
define float @tgt_maxnum3200() {
  ret float 1.0
}

define float @src_maxnum3201() {
  %n = bitcast i32 2139095041 to float
  %r = call float @llvm.maxnum.f32(float %n, float 1.0)
  ret float %r
}
define float @tgt_maxnum3201() {
  %n = bitcast i32 2143289345 to float
  ret float %n
}

define float @src_maxnum3210() {
  %n = bitcast i32 2139095041 to float
  %r = call float @llvm.maxnum.f32(float 1.0, float %n)
  ret float %r
}
define float @tgt_maxnum3210() {
  ret float 1.0
}

define float @src_maxnum3211() {
  %n = bitcast i32 2139095041 to float
  %r = call float @llvm.maxnum.f32(float 1.0, float %n)
  ret float %r
}
define float @tgt_maxnum3211() {
  %n = bitcast i32 2143289345 to float
  ret float %n
}

define double @src_maxnum6400() {
  %n = bitcast i64 9218868437227405313 to double
  %r = call double @llvm.maxnum.f64(double %n, double 1.0)
  ret double %r
}
define double @tgt_maxnum6400() {
  ret double 1.0
}

define double @src_maxnum6401() {
  %n = bitcast i64 9218868437227405313 to double
  %r = call double @llvm.maxnum.f64(double %n, double 1.0)
  ret double %r
}
define double @tgt_maxnum6401() {
  %n = bitcast i64 9221120237041090561 to double
  ret double %n
}

define double @src_maxnum6410() {
  %n = bitcast i64 9218868437227405313 to double
  %r = call double @llvm.maxnum.f64(double 1.0, double %n)
  ret double %r
}
define double @tgt_maxnum6410() {
  ret double 1.0
}

define double @src_maxnum6411() {
  %n = bitcast i64 9218868437227405313 to double
  %r = call double @llvm.maxnum.f64(double 1.0, double %n)
  ret double %r
}
define double @tgt_maxnum6411() {
  %n = bitcast i64 9221120237041090561 to double
  ret double %n
}
