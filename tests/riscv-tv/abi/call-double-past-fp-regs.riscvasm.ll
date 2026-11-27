; CHECK: 1 correct transformations

; The ninth double has no FP argument register left, so LP64D passes it in a0.
declare void @consume(double, double, double, double, double, double, double,
                      double, double)
define void @test() {
  %anchor = add i64 0, 0
  call void @consume(double 1.0, double 1.0, double 1.0, double 1.0,
                     double 1.0, double 1.0, double 1.0, double 1.0,
                     double 2.0)
  ret void
}
