; rs1 = x0, rd != x0 sets vl to VLMAX = 4 * 128 / 16
define i64 @test() {
  ret i64 32
}
