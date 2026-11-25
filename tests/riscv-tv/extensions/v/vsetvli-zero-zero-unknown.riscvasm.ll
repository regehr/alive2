; CHECK: ERROR: vsetvli zero, zero, ... needs the previous vtype to be statically known
define i64 @test() {
  ret i64 208
}
