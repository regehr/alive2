; An sNaN with a number may produce either the number or a NaN.
; CHECK: 16 correct transformations
; CHECK: 0 incorrect transformations
; CHECK: 0 failed-to-prove transformations

define float @src_minnum_f32_nan_first_number() {
  %n = bitcast i32 2139095041 to float
  %r = call float @llvm.minnum.f32(float %n, float 1.0)
  ret float %r
}
define float @tgt_minnum_f32_nan_first_number() {
  ret float 1.0
}

define float @src_minnum_f32_nan_first_nan() {
  %n = bitcast i32 2139095041 to float
  %r = call float @llvm.minnum.f32(float %n, float 1.0)
  ret float %r
}
define float @tgt_minnum_f32_nan_first_nan() {
  %q = bitcast i32 2143289345 to float
  ret float %q
}

define float @src_minnum_f32_nan_second_number() {
  %n = bitcast i32 2139095041 to float
  %r = call float @llvm.minnum.f32(float 1.0, float %n)
  ret float %r
}
define float @tgt_minnum_f32_nan_second_number() {
  ret float 1.0
}

define float @src_minnum_f32_nan_second_nan() {
  %n = bitcast i32 2139095041 to float
  %r = call float @llvm.minnum.f32(float 1.0, float %n)
  ret float %r
}
define float @tgt_minnum_f32_nan_second_nan() {
  %q = bitcast i32 2143289345 to float
  ret float %q
}

define double @src_minnum_f64_nan_first_number() {
  %n = bitcast i64 9218868437227405313 to double
  %r = call double @llvm.minnum.f64(double %n, double 1.0)
  ret double %r
}
define double @tgt_minnum_f64_nan_first_number() {
  ret double 1.0
}

define double @src_minnum_f64_nan_first_nan() {
  %n = bitcast i64 9218868437227405313 to double
  %r = call double @llvm.minnum.f64(double %n, double 1.0)
  ret double %r
}
define double @tgt_minnum_f64_nan_first_nan() {
  %q = bitcast i64 9221120237041090561 to double
  ret double %q
}

define double @src_minnum_f64_nan_second_number() {
  %n = bitcast i64 9218868437227405313 to double
  %r = call double @llvm.minnum.f64(double 1.0, double %n)
  ret double %r
}
define double @tgt_minnum_f64_nan_second_number() {
  ret double 1.0
}

define double @src_minnum_f64_nan_second_nan() {
  %n = bitcast i64 9218868437227405313 to double
  %r = call double @llvm.minnum.f64(double 1.0, double %n)
  ret double %r
}
define double @tgt_minnum_f64_nan_second_nan() {
  %q = bitcast i64 9221120237041090561 to double
  ret double %q
}

define float @src_maxnum_f32_nan_first_number() {
  %n = bitcast i32 2139095041 to float
  %r = call float @llvm.maxnum.f32(float %n, float 1.0)
  ret float %r
}
define float @tgt_maxnum_f32_nan_first_number() {
  ret float 1.0
}

define float @src_maxnum_f32_nan_first_nan() {
  %n = bitcast i32 2139095041 to float
  %r = call float @llvm.maxnum.f32(float %n, float 1.0)
  ret float %r
}
define float @tgt_maxnum_f32_nan_first_nan() {
  %q = bitcast i32 2143289345 to float
  ret float %q
}

define float @src_maxnum_f32_nan_second_number() {
  %n = bitcast i32 2139095041 to float
  %r = call float @llvm.maxnum.f32(float 1.0, float %n)
  ret float %r
}
define float @tgt_maxnum_f32_nan_second_number() {
  ret float 1.0
}

define float @src_maxnum_f32_nan_second_nan() {
  %n = bitcast i32 2139095041 to float
  %r = call float @llvm.maxnum.f32(float 1.0, float %n)
  ret float %r
}
define float @tgt_maxnum_f32_nan_second_nan() {
  %q = bitcast i32 2143289345 to float
  ret float %q
}

define double @src_maxnum_f64_nan_first_number() {
  %n = bitcast i64 9218868437227405313 to double
  %r = call double @llvm.maxnum.f64(double %n, double 1.0)
  ret double %r
}
define double @tgt_maxnum_f64_nan_first_number() {
  ret double 1.0
}

define double @src_maxnum_f64_nan_first_nan() {
  %n = bitcast i64 9218868437227405313 to double
  %r = call double @llvm.maxnum.f64(double %n, double 1.0)
  ret double %r
}
define double @tgt_maxnum_f64_nan_first_nan() {
  %q = bitcast i64 9221120237041090561 to double
  ret double %q
}

define double @src_maxnum_f64_nan_second_number() {
  %n = bitcast i64 9218868437227405313 to double
  %r = call double @llvm.maxnum.f64(double 1.0, double %n)
  ret double %r
}
define double @tgt_maxnum_f64_nan_second_number() {
  ret double 1.0
}

define double @src_maxnum_f64_nan_second_nan() {
  %n = bitcast i64 9218868437227405313 to double
  %r = call double @llvm.maxnum.f64(double 1.0, double %n)
  ret double %r
}
define double @tgt_maxnum_f64_nan_second_nan() {
  %q = bitcast i64 9221120237041090561 to double
  ret double %q
}

declare double @llvm.maxnum.f64(double, double)
declare double @llvm.minnum.f64(double, double)
declare float @llvm.maxnum.f32(float, float)
declare float @llvm.minnum.f32(float, float)
