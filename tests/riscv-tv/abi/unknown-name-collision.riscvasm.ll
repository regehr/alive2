; Source functions with helper-like names must keep their names and calls.
declare void @__backend_tv_unknown_i64()
define i64 @test() {
  %anchor = add i64 0, 0
  call void @__backend_tv_unknown_i64()
  ret i64 0
}
