define <4 x i32> @f(double %0) {
  %2 = fptosi double %0 to i32
  %3 = insertelement <4 x i32> zeroinitializer, i32 %2, i64 0
  ret <4 x i32> %3
}

; _f:
;	movi.2d	v1, #0000000000000000
;	fcvtzs	w8, d0
;	mov.s	v1[0], w8
;	mov.16b	v0, v1
;	ret
