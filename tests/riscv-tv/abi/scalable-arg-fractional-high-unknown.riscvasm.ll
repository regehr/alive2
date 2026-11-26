; CHECK: 1 incorrect transformations
; only the low part of an mf2 argument's register is specified. this
; slides the high half of v8 down into the returned elements, which would
; verify if those bits were assumed to be zero
define <vscale x 1 x i32> @test(<vscale x 1 x i32> %a) {
  ret <vscale x 1 x i32> zeroinitializer
}
