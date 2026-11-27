; c.add rd, rs2
define i64 @test(i64 %x, i64 %y) {
  %r = add i64 %x, %y
  ret i64 %r
}
