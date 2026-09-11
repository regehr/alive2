; Unlike generic nsz, min/max must preserve equal-sign zeros.
; ERROR: Value mismatch
; CHECK: 0 correct transformations
; CHECK: 12 incorrect transformations
; CHECK: 0 failed-to-prove transformations

define float @src_minnum_pos() {
  %r = call nsz float @llvm.minnum.f32(float 0.0, float 0.0)
  ret float %r
}
define float @tgt_minnum_pos() {
  ret float -0.0
}

define float @src_minnum_neg() {
  %r = call nsz float @llvm.minnum.f32(float -0.0, float -0.0)
  ret float %r
}
define float @tgt_minnum_neg() {
  ret float 0.0
}

define float @src_maxnum_pos() {
  %r = call nsz float @llvm.maxnum.f32(float 0.0, float 0.0)
  ret float %r
}
define float @tgt_maxnum_pos() {
  ret float -0.0
}

define float @src_maxnum_neg() {
  %r = call nsz float @llvm.maxnum.f32(float -0.0, float -0.0)
  ret float %r
}
define float @tgt_maxnum_neg() {
  ret float 0.0
}

define float @src_minimum_pos() {
  %r = call nsz float @llvm.minimum.f32(float 0.0, float 0.0)
  ret float %r
}
define float @tgt_minimum_pos() {
  ret float -0.0
}

define float @src_minimum_neg() {
  %r = call nsz float @llvm.minimum.f32(float -0.0, float -0.0)
  ret float %r
}
define float @tgt_minimum_neg() {
  ret float 0.0
}

define float @src_maximum_pos() {
  %r = call nsz float @llvm.maximum.f32(float 0.0, float 0.0)
  ret float %r
}
define float @tgt_maximum_pos() {
  ret float -0.0
}

define float @src_maximum_neg() {
  %r = call nsz float @llvm.maximum.f32(float -0.0, float -0.0)
  ret float %r
}
define float @tgt_maximum_neg() {
  ret float 0.0
}

define float @src_minimumnum_pos() {
  %r = call nsz float @llvm.minimumnum.f32(float 0.0, float 0.0)
  ret float %r
}
define float @tgt_minimumnum_pos() {
  ret float -0.0
}

define float @src_minimumnum_neg() {
  %r = call nsz float @llvm.minimumnum.f32(float -0.0, float -0.0)
  ret float %r
}
define float @tgt_minimumnum_neg() {
  ret float 0.0
}

define float @src_maximumnum_pos() {
  %r = call nsz float @llvm.maximumnum.f32(float 0.0, float 0.0)
  ret float %r
}
define float @tgt_maximumnum_pos() {
  ret float -0.0
}

define float @src_maximumnum_neg() {
  %r = call nsz float @llvm.maximumnum.f32(float -0.0, float -0.0)
  ret float %r
}
define float @tgt_maximumnum_neg() {
  ret float 0.0
}

declare float @llvm.maximum.f32(float, float)
declare float @llvm.maximumnum.f32(float, float)
declare float @llvm.maxnum.f32(float, float)
declare float @llvm.minimum.f32(float, float)
declare float @llvm.minimumnum.f32(float, float)
declare float @llvm.minnum.f32(float, float)
