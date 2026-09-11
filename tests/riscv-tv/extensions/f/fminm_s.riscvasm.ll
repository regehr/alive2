; Check every input bit pattern, including NaNs, infinities and signed zeros.
; Resolve equal operands by bits: min(-0,+0)=-0 and max(-0,+0)=+0.
define i64 @fminm_s(float noundef %a, float noundef %b) {
  %abits = bitcast float %a to i32
  %bbits = bitcast float %b to i32
  %nan = fcmp uno float %a, %b
  %choose_a = fcmp olt float %a, %b
  %chosen = select i1 %choose_a, i32 %abits, i32 %bbits
  %equal = fcmp oeq float %a, %b
  %tie = or i32 %abits, %bbits
  %ordered = select i1 %equal, i32 %tie, i32 %chosen
  %bits = select i1 %nan, i32 2143289344, i32 %ordered
  %result = sext i32 %bits to i64
  ret i64 %result
}
