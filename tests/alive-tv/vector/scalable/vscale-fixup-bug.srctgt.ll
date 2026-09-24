; TEST-ARGS: --max-vscale=4
; ERROR: Target is more poisonous than source

; Correct at vscale = 1, where index 2 is out of bounds, but not at larger
; scales.
define i8 @src(<vscale x 2 x i8> %v) {
  %r = extractelement <vscale x 2 x i8> %v, i32 2
  ret i8 %r
}

define i8 @tgt(<vscale x 2 x i8> %v) {
  ret i8 poison
}
