; FADD honors directed rounding.
define void @fadd_rounding(ptr noundef %out) {
  ; 1+2^-24, RUP, is the next single above 1.
  %p0 = getelementptr i64, ptr %out, i64 0
  store i64 1065353217, ptr %p0, align 1
  ; -1-2^-24, RDN, is the next single below -1.
  %p1 = getelementptr i64, ptr %out, i64 1
  store i64 -1082130431, ptr %p1, align 1
  ret void
}
