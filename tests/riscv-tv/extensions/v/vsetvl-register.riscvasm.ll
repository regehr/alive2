; CHECK: ERROR: vsetvl takes vtype from a register
define i64 @test(i64 %a, i64 %b) {
  ret i64 %a
}
