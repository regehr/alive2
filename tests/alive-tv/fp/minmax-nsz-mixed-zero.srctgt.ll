; CHECK: 24 correct transformations
; CHECK: 0 incorrect transformations

define float @src_minnum00() {
  %r = call nsz float @llvm.minnum.f32(float 0.0, float -0.0)
  ret float %r
}
define float @tgt_minnum00() {
  ret float 0.0
}

define float @src_minnum01() {
  %r = call nsz float @llvm.minnum.f32(float 0.0, float -0.0)
  ret float %r
}
define float @tgt_minnum01() {
  ret float -0.0
}

define float @src_minnum10() {
  %r = call nsz float @llvm.minnum.f32(float -0.0, float 0.0)
  ret float %r
}
define float @tgt_minnum10() {
  ret float 0.0
}

define float @src_minnum11() {
  %r = call nsz float @llvm.minnum.f32(float -0.0, float 0.0)
  ret float %r
}
define float @tgt_minnum11() {
  ret float -0.0
}

define float @src_maxnum00() {
  %r = call nsz float @llvm.maxnum.f32(float 0.0, float -0.0)
  ret float %r
}
define float @tgt_maxnum00() {
  ret float 0.0
}

define float @src_maxnum01() {
  %r = call nsz float @llvm.maxnum.f32(float 0.0, float -0.0)
  ret float %r
}
define float @tgt_maxnum01() {
  ret float -0.0
}

define float @src_maxnum10() {
  %r = call nsz float @llvm.maxnum.f32(float -0.0, float 0.0)
  ret float %r
}
define float @tgt_maxnum10() {
  ret float 0.0
}

define float @src_maxnum11() {
  %r = call nsz float @llvm.maxnum.f32(float -0.0, float 0.0)
  ret float %r
}
define float @tgt_maxnum11() {
  ret float -0.0
}

define float @src_minimum00() {
  %r = call nsz float @llvm.minimum.f32(float 0.0, float -0.0)
  ret float %r
}
define float @tgt_minimum00() {
  ret float 0.0
}

define float @src_minimum01() {
  %r = call nsz float @llvm.minimum.f32(float 0.0, float -0.0)
  ret float %r
}
define float @tgt_minimum01() {
  ret float -0.0
}

define float @src_minimum10() {
  %r = call nsz float @llvm.minimum.f32(float -0.0, float 0.0)
  ret float %r
}
define float @tgt_minimum10() {
  ret float 0.0
}

define float @src_minimum11() {
  %r = call nsz float @llvm.minimum.f32(float -0.0, float 0.0)
  ret float %r
}
define float @tgt_minimum11() {
  ret float -0.0
}

define float @src_maximum00() {
  %r = call nsz float @llvm.maximum.f32(float 0.0, float -0.0)
  ret float %r
}
define float @tgt_maximum00() {
  ret float 0.0
}

define float @src_maximum01() {
  %r = call nsz float @llvm.maximum.f32(float 0.0, float -0.0)
  ret float %r
}
define float @tgt_maximum01() {
  ret float -0.0
}

define float @src_maximum10() {
  %r = call nsz float @llvm.maximum.f32(float -0.0, float 0.0)
  ret float %r
}
define float @tgt_maximum10() {
  ret float 0.0
}

define float @src_maximum11() {
  %r = call nsz float @llvm.maximum.f32(float -0.0, float 0.0)
  ret float %r
}
define float @tgt_maximum11() {
  ret float -0.0
}

define float @src_minimumnum00() {
  %r = call nsz float @llvm.minimumnum.f32(float 0.0, float -0.0)
  ret float %r
}
define float @tgt_minimumnum00() {
  ret float 0.0
}

define float @src_minimumnum01() {
  %r = call nsz float @llvm.minimumnum.f32(float 0.0, float -0.0)
  ret float %r
}
define float @tgt_minimumnum01() {
  ret float -0.0
}

define float @src_minimumnum10() {
  %r = call nsz float @llvm.minimumnum.f32(float -0.0, float 0.0)
  ret float %r
}
define float @tgt_minimumnum10() {
  ret float 0.0
}

define float @src_minimumnum11() {
  %r = call nsz float @llvm.minimumnum.f32(float -0.0, float 0.0)
  ret float %r
}
define float @tgt_minimumnum11() {
  ret float -0.0
}

define float @src_maximumnum00() {
  %r = call nsz float @llvm.maximumnum.f32(float 0.0, float -0.0)
  ret float %r
}
define float @tgt_maximumnum00() {
  ret float 0.0
}

define float @src_maximumnum01() {
  %r = call nsz float @llvm.maximumnum.f32(float 0.0, float -0.0)
  ret float %r
}
define float @tgt_maximumnum01() {
  ret float -0.0
}

define float @src_maximumnum10() {
  %r = call nsz float @llvm.maximumnum.f32(float -0.0, float 0.0)
  ret float %r
}
define float @tgt_maximumnum10() {
  ret float 0.0
}

define float @src_maximumnum11() {
  %r = call nsz float @llvm.maximumnum.f32(float -0.0, float 0.0)
  ret float %r
}
define float @tgt_maximumnum11() {
  ret float -0.0
}
