; Check instruction semantics independently of LLVM min/max intrinsics.
; One NaN (quiet or signaling) selects the number; two NaNs give a canonical NaN.
; Compare result bits so signed zeros and NaN payloads are observable.
define i64 @fmax_h(half noundef %a, half noundef %b) {
  %abits = bitcast half %a to i16
  %bbits = bitcast half %b to i16
  %a_nan = fcmp uno half %a, %a
  %b_nan = fcmp uno half %b, %b
  %choose_a = fcmp ogt half %a, %b
  %chosen = select i1 %choose_a, i16 %abits, i16 %bbits
  %equal = fcmp oeq half %a, %b
  %tie = and i16 %abits, %bbits
  %ordered = select i1 %equal, i16 %tie, i16 %chosen
  %b_checked = select i1 %b_nan, i16 %abits, i16 %ordered
  %a_checked = select i1 %a_nan, i16 %bbits, i16 %b_checked
  %both_nan = and i1 %a_nan, %b_nan
  %bits = select i1 %both_nan, i16 32256, i16 %a_checked
  %result = sext i16 %bits to i64
  ret i64 %result
}
