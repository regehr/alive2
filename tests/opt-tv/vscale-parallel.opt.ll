; TEST-ARGS: -passes=tv,instcombine,tv -tv-max-vscale=4 -tv-parallel=unrestricted -max-subprocesses=2
; CHECK: Checking vscale = 1
; CHECK: Checking vscale = 2
; CHECK: Checking vscale = 4
; CHECK: Transformation seems to be correct!
; CHECK-NOT: ERROR:
; CHECK-NOT: Transformation doesn't verify!

; Each child verifies a concrete typing, independently of the parent's
; other cached scales and subsequent SMT context resets.
define <vscale x 2 x i32> @f(<vscale x 2 x i32> %x) {
  %b = add <vscale x 2 x i32> %x, zeroinitializer
  ret <vscale x 2 x i32> %b
}
