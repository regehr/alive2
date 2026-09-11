; FDIV honors directed rounding.
define void @fdiv_rounding(ptr noundef %out) {
  ; 1/3, RTZ, is the lower adjacent single.
  %p0 = getelementptr i64, ptr %out, i64 0
  store i64 1051372202, ptr %p0, align 1
  ret void
}
