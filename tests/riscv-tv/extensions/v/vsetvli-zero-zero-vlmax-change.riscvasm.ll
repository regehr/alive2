; CHECK: ERROR: vsetvli zero, zero, ... changes VLMAX, which is reserved
define i64 @test() {
  ret i64 3
}
