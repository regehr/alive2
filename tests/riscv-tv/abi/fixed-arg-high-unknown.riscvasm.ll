; CHECK: 1 incorrect transformations
; only the low 64 bits of a <2 x i32> argument's register are specified.
; this slides the next 64 bits down into the result, which would verify if
; those bits were assumed to be zero
define <2 x i32> @test(<2 x i32> %a) {
  ret <2 x i32> zeroinitializer
}
