; Zfa: truncate toward zero, reduce modulo 2^32, and sign-extend.
; Infinities and NaNs map to zero. Expected values use integer truncation/modulo.
define void @fcvtmod_w_d(ptr noundef %out) {
  ; +0 (0x0000000000000000) -> 0
  %p0 = getelementptr i64, ptr %out, i64 0
  store i64 0, ptr %p0, align 1
  ; -0 (0x8000000000000000) -> 0
  %p1 = getelementptr i64, ptr %out, i64 1
  store i64 0, ptr %p1, align 1
  ; minimum positive subnormal (0x0000000000000001) -> 0
  %p2 = getelementptr i64, ptr %out, i64 2
  store i64 0, ptr %p2, align 1
  ; minimum negative subnormal (0x8000000000000001) -> 0
  %p3 = getelementptr i64, ptr %out, i64 3
  store i64 0, ptr %p3, align 1
  ; 0.75 (0x3fe8000000000000) -> 0
  %p4 = getelementptr i64, ptr %out, i64 4
  store i64 0, ptr %p4, align 1
  ; -0.75 (0xbfe8000000000000) -> 0
  %p5 = getelementptr i64, ptr %out, i64 5
  store i64 0, ptr %p5, align 1
  ; 1.75 (0x3ffc000000000000) -> 1
  %p6 = getelementptr i64, ptr %out, i64 6
  store i64 1, ptr %p6, align 1
  ; -1.75 (0xbffc000000000000) -> -1
  %p7 = getelementptr i64, ptr %out, i64 7
  store i64 -1, ptr %p7, align 1
  ; 2^31-0.5 (0x41dfffffffe00000) -> 2147483647
  %p8 = getelementptr i64, ptr %out, i64 8
  store i64 2147483647, ptr %p8, align 1
  ; 2^31+0.5 (0x41e0000000100000) -> -2147483648
  %p9 = getelementptr i64, ptr %out, i64 9
  store i64 -2147483648, ptr %p9, align 1
  ; -2^31-0.5 (0xc1e0000000100000) -> -2147483648
  %p10 = getelementptr i64, ptr %out, i64 10
  store i64 -2147483648, ptr %p10, align 1
  ; 2^32-0.5 (0x41effffffff00000) -> -1
  %p11 = getelementptr i64, ptr %out, i64 11
  store i64 -1, ptr %p11, align 1
  ; -2^32+0.5 (0xc1effffffff00000) -> 1
  %p12 = getelementptr i64, ptr %out, i64 12
  store i64 1, ptr %p12, align 1
  ; 2^32+1.75 (0x41f00000001c0000) -> 1
  %p13 = getelementptr i64, ptr %out, i64 13
  store i64 1, ptr %p13, align 1
  ; -2^32-1.75 (0xc1f00000001c0000) -> -1
  %p14 = getelementptr i64, ptr %out, i64 14
  store i64 -1, ptr %p14, align 1
  ; 2^51+1.5 (0x4320000000000003) -> 1
  %p15 = getelementptr i64, ptr %out, i64 15
  store i64 1, ptr %p15, align 1
  ; 2^52+1 (0x4330000000000001) -> 1
  %p16 = getelementptr i64, ptr %out, i64 16
  store i64 1, ptr %p16, align 1
  ; 2^53-1 (0x433fffffffffffff) -> -1
  %p17 = getelementptr i64, ptr %out, i64 17
  store i64 -1, ptr %p17, align 1
  ; 2^53+2 (0x4340000000000001) -> 2
  %p18 = getelementptr i64, ptr %out, i64 18
  store i64 2, ptr %p18, align 1
  ; 2^63+2048 (0x43e0000000000001) -> 2048
  %p19 = getelementptr i64, ptr %out, i64 19
  store i64 2048, ptr %p19, align 1
  ; 2^64+4096 (0x43f0000000000001) -> 4096
  %p20 = getelementptr i64, ptr %out, i64 20
  store i64 4096, ptr %p20, align 1
  ; -2^64-4096 (0xc3f0000000000001) -> -4096
  %p21 = getelementptr i64, ptr %out, i64 21
  store i64 -4096, ptr %p21, align 1
  ; 2^83+2^31 (0x4520000000000001) -> -2147483648
  %p22 = getelementptr i64, ptr %out, i64 22
  store i64 -2147483648, ptr %p22, align 1
  ; -2^83-2^31 (0xc520000000000001) -> -2147483648
  %p23 = getelementptr i64, ptr %out, i64 23
  store i64 -2147483648, ptr %p23, align 1
  ; largest double below 2^84 (0x452fffffffffffff) -> -2147483648
  %p24 = getelementptr i64, ptr %out, i64 24
  store i64 -2147483648, ptr %p24, align 1
  ; 2^84+2^32 (0x4530000000000001) -> 0
  %p25 = getelementptr i64, ptr %out, i64 25
  store i64 0, ptr %p25, align 1
  ; maximum finite double (0x7fefffffffffffff) -> 0
  %p26 = getelementptr i64, ptr %out, i64 26
  store i64 0, ptr %p26, align 1
  ; minimum finite double (0xffefffffffffffff) -> 0
  %p27 = getelementptr i64, ptr %out, i64 27
  store i64 0, ptr %p27, align 1
  ; +infinity (0x7ff0000000000000) -> 0
  %p28 = getelementptr i64, ptr %out, i64 28
  store i64 0, ptr %p28, align 1
  ; -infinity (0xfff0000000000000) -> 0
  %p29 = getelementptr i64, ptr %out, i64 29
  store i64 0, ptr %p29, align 1
  ; quiet NaN with payload (0x7ff8000000000123) -> 0
  %p30 = getelementptr i64, ptr %out, i64 30
  store i64 0, ptr %p30, align 1
  ; negative quiet NaN (0xfff8000000000123) -> 0
  %p31 = getelementptr i64, ptr %out, i64 31
  store i64 0, ptr %p31, align 1
  ; signaling NaN (0x7ff0000000000001) -> 0
  %p32 = getelementptr i64, ptr %out, i64 32
  store i64 0, ptr %p32, align 1
  ; negative signaling NaN (0xfff0000000000001) -> 0
  %p33 = getelementptr i64, ptr %out, i64 33
  store i64 0, ptr %p33, align 1
  ret void
}
