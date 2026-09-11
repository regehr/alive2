; Without nsz, all six intrinsics order -0.0 below +0.0.
; ERROR: Value mismatch
; CHECK: 0 correct transformations
; CHECK: 12 incorrect transformations
; CHECK: 0 failed-to-prove transformations

define float @src_minnum_pos_neg() {
  %r = call float @llvm.minnum.f32(float 0.0, float -0.0)
  ret float %r
}
define float @tgt_minnum_pos_neg() {
  ret float 0.0
}

define float @src_minnum_neg_pos() {
  %r = call float @llvm.minnum.f32(float -0.0, float 0.0)
  ret float %r
}
define float @tgt_minnum_neg_pos() {
  ret float 0.0
}

define float @src_maxnum_pos_neg() {
  %r = call float @llvm.maxnum.f32(float 0.0, float -0.0)
  ret float %r
}
define float @tgt_maxnum_pos_neg() {
  ret float -0.0
}

define float @src_maxnum_neg_pos() {
  %r = call float @llvm.maxnum.f32(float -0.0, float 0.0)
  ret float %r
}
define float @tgt_maxnum_neg_pos() {
  ret float -0.0
}

define float @src_minimum_pos_neg() {
  %r = call float @llvm.minimum.f32(float 0.0, float -0.0)
  ret float %r
}
define float @tgt_minimum_pos_neg() {
  ret float 0.0
}

define float @src_minimum_neg_pos() {
  %r = call float @llvm.minimum.f32(float -0.0, float 0.0)
  ret float %r
}
define float @tgt_minimum_neg_pos() {
  ret float 0.0
}

define float @src_maximum_pos_neg() {
  %r = call float @llvm.maximum.f32(float 0.0, float -0.0)
  ret float %r
}
define float @tgt_maximum_pos_neg() {
  ret float -0.0
}

define float @src_maximum_neg_pos() {
  %r = call float @llvm.maximum.f32(float -0.0, float 0.0)
  ret float %r
}
define float @tgt_maximum_neg_pos() {
  ret float -0.0
}

define float @src_minimumnum_pos_neg() {
  %r = call float @llvm.minimumnum.f32(float 0.0, float -0.0)
  ret float %r
}
define float @tgt_minimumnum_pos_neg() {
  ret float 0.0
}

define float @src_minimumnum_neg_pos() {
  %r = call float @llvm.minimumnum.f32(float -0.0, float 0.0)
  ret float %r
}
define float @tgt_minimumnum_neg_pos() {
  ret float 0.0
}

define float @src_maximumnum_pos_neg() {
  %r = call float @llvm.maximumnum.f32(float 0.0, float -0.0)
  ret float %r
}
define float @tgt_maximumnum_pos_neg() {
  ret float -0.0
}

define float @src_maximumnum_neg_pos() {
  %r = call float @llvm.maximumnum.f32(float -0.0, float 0.0)
  ret float %r
}
define float @tgt_maximumnum_neg_pos() {
  ret float -0.0
}

declare float @llvm.maximum.f32(float, float)
declare float @llvm.maximumnum.f32(float, float)
declare float @llvm.maxnum.f32(float, float)
declare float @llvm.minimum.f32(float, float)
declare float @llvm.minimumnum.f32(float, float)
declare float @llvm.minnum.f32(float, float)
