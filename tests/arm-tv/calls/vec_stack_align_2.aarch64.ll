
declare fastcc i64 @g1(double, <4 x i32>, double, <4 x i32>, double, <4 x i32>, double, <4 x i32>, <4 x i32>, double, <4 x i32>)

define void @f() {
  %1 = call fastcc i64 @g1(double 0.000000e+00, <4 x i32> zeroinitializer, double 0.000000e+00, <4 x i32> zeroinitializer, double 0.000000e+00, <4 x i32> zeroinitializer, double 0.000000e+00, <4 x i32> zeroinitializer, <4 x i32> zeroinitializer, double 0.000000e+00, <4 x i32> zeroinitializer)
  ret void
}
