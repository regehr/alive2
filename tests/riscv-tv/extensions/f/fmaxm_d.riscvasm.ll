; Check every input bit pattern, including NaNs, infinities and signed zeros.
; Resolve equal operands by bits: min(-0,+0)=-0 and max(-0,+0)=+0.
define i64 @fmaxm_d(double noundef %a, double noundef %b) {
  %abits = bitcast double %a to i64
  %bbits = bitcast double %b to i64
  %nan = fcmp uno double %a, %b
  %choose_a = fcmp ogt double %a, %b
  %chosen = select i1 %choose_a, i64 %abits, i64 %bbits
  %equal = fcmp oeq double %a, %b
  %tie = and i64 %abits, %bbits
  %ordered = select i1 %equal, i64 %tie, i64 %chosen
  %bits = select i1 %nan, i64 9221120237041090560, i64 %ordered
  ret i64 %bits
}
