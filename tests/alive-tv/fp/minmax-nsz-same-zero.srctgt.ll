; ERROR: Value mismatch
; CHECK: 12 incorrect transformations
; nsz must preserve the sign when both inputs are the same zero.

define float @src_minnum0() {
  %r = call nsz float @llvm.minnum.f32(float 0.0, float 0.0)
  ret float %r
}
define float @tgt_minnum0() {
  ret float -0.0
}

define float @src_minnum1() {
  %r = call nsz float @llvm.minnum.f32(float -0.0, float -0.0)
  ret float %r
}
define float @tgt_minnum1() {
  ret float 0.0
}

define float @src_maxnum0() {
  %r = call nsz float @llvm.maxnum.f32(float 0.0, float 0.0)
  ret float %r
}
define float @tgt_maxnum0() {
  ret float -0.0
}

define float @src_maxnum1() {
  %r = call nsz float @llvm.maxnum.f32(float -0.0, float -0.0)
  ret float %r
}
define float @tgt_maxnum1() {
  ret float 0.0
}

define float @src_minimum0() {
  %r = call nsz float @llvm.minimum.f32(float 0.0, float 0.0)
  ret float %r
}
define float @tgt_minimum0() {
  ret float -0.0
}

define float @src_minimum1() {
  %r = call nsz float @llvm.minimum.f32(float -0.0, float -0.0)
  ret float %r
}
define float @tgt_minimum1() {
  ret float 0.0
}

define float @src_maximum0() {
  %r = call nsz float @llvm.maximum.f32(float 0.0, float 0.0)
  ret float %r
}
define float @tgt_maximum0() {
  ret float -0.0
}

define float @src_maximum1() {
  %r = call nsz float @llvm.maximum.f32(float -0.0, float -0.0)
  ret float %r
}
define float @tgt_maximum1() {
  ret float 0.0
}

define float @src_minimumnum0() {
  %r = call nsz float @llvm.minimumnum.f32(float 0.0, float 0.0)
  ret float %r
}
define float @tgt_minimumnum0() {
  ret float -0.0
}

define float @src_minimumnum1() {
  %r = call nsz float @llvm.minimumnum.f32(float -0.0, float -0.0)
  ret float %r
}
define float @tgt_minimumnum1() {
  ret float 0.0
}

define float @src_maximumnum0() {
  %r = call nsz float @llvm.maximumnum.f32(float 0.0, float 0.0)
  ret float %r
}
define float @tgt_maximumnum0() {
  ret float -0.0
}

define float @src_maximumnum1() {
  %r = call nsz float @llvm.maximumnum.f32(float -0.0, float -0.0)
  ret float %r
}
define float @tgt_maximumnum1() {
  ret float 0.0
}
