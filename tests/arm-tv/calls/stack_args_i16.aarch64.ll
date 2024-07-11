declare i16 @g(i16, i16, i16, i16, i16, i16, i16, i16, i16, i16, i16, i16, i16, i16, i16)

define i16 @f() {
  %x = call i16 @g(i16 0, i16 1, i16 2, i16 3, i16 4, i16 5, i16 6, i16 7, i16 8, i16 9, i16 10, i16 11, i16 12, i16 13, i16 14)
  ret i16 %x
}
