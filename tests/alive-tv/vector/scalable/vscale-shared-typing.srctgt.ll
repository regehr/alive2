; TEST-ARGS: --smt-verbose --smt-random-seed=42
; CHECK: (declare-fun vscale () (_ BitVec 5))
; CHECK: Checking vscale = 1
; CHECK: Checking vscale = 2
; CHECK: Checking vscale = 4
; CHECK: Checking vscale = 8
; CHECK: Checking vscale = 16
; CHECK: Transformation seems to be correct! (all applicable vscale values up to 16)
; CHECK-NOT: ERROR:

; Both vector types and llvm.vscale must use the same selected scale,
; including through a bitcast with different element widths.
define i16 @src() {
  %v = call i32 @llvm.vscale.i32()
  %n = mul i32 %v, 2
  %last = sub i32 %n, 1
  %a = insertelement <vscale x 2 x i16> poison, i16 -1, i32 %last
  %b = bitcast <vscale x 2 x i16> %a to <vscale x 4 x i8>
  %m = mul i32 %v, 4
  %lastbyte = sub i32 %m, 1
  %x = extractelement <vscale x 4 x i8> %b, i32 %lastbyte
  %r = sext i8 %x to i16
  ret i16 %r
}

define i16 @tgt() {
  ret i16 -1
}
