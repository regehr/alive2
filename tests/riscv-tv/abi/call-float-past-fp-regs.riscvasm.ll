; XFAIL: we don't support call arguments past the FP registers yet

; The ninth float has no FP argument register left, so LP64D passes it in
; a0 -- llc emits "fmv.w.x fa0, a0" for a callee reading it back. The
; callee side of our ABI already handles a float arriving in a GPR; this,
; the caller side, does not yet.
declare void @consume(float, float, float, float, float, float, float, float,
                      float)
define void @test() {
  %anchor = add i64 0, 0
  call void @consume(float 1.0, float 1.0, float 1.0, float 1.0, float 1.0,
                     float 1.0, float 1.0, float 1.0, float 1.0)
  ret void
}
