; FSQRT honors directed rounding.
define void @fsqrt_rounding(ptr noundef %out) {
  ; sqrt(2), RUP, is the upper adjacent single.
  %p0 = getelementptr i64, ptr %out, i64 0
  store i64 1068827892, ptr %p0, align 1
  ret void
}
