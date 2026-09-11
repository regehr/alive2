; Check instruction semantics independently of LLVM min/max intrinsics.
; One NaN (quiet or signaling) selects the number; two NaNs give a canonical NaN.
; Compare result bits so signed zeros and NaN payloads are observable.
define i64 @fmax_d(double noundef %a, double noundef %b) {
  %abits = bitcast double %a to i64
  %bbits = bitcast double %b to i64
  %a_nan = fcmp uno double %a, %a
  %b_nan = fcmp uno double %b, %b
  %choose_a = fcmp ogt double %a, %b
  %chosen = select i1 %choose_a, i64 %abits, i64 %bbits
  %equal = fcmp oeq double %a, %b
  %tie = and i64 %abits, %bbits
  %ordered = select i1 %equal, i64 %tie, i64 %chosen
  %b_checked = select i1 %b_nan, i64 %abits, i64 %ordered
  %a_checked = select i1 %a_nan, i64 %bbits, i64 %b_checked
  %both_nan = and i1 %a_nan, %b_nan
  %bits = select i1 %both_nan, i64 9221120237041090560, i64 %a_checked
  ret i64 %bits
}
