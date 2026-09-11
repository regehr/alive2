; All four fused multiply-add forms round after applying their operand signs.
define void @fma_rounding(ptr noundef %out) {
  ; FMADD: 1*1+2^-24, RUP.
  %p0 = getelementptr i64, ptr %out, i64 0
  store i64 1065353217, ptr %p0, align 1
  ; FMSUB: 1*1-(-2^-24), RUP.
  %p1 = getelementptr i64, ptr %out, i64 1
  store i64 1065353217, ptr %p1, align 1
  ; FNMADD: -(1*1)-2^-24, RDN.
  %p2 = getelementptr i64, ptr %out, i64 2
  store i64 -1082130431, ptr %p2, align 1
  ; FNMSUB: -(1*1)+(-2^-24), RDN.
  %p3 = getelementptr i64, ptr %out, i64 3
  store i64 -1082130431, ptr %p3, align 1
  ret void
}
