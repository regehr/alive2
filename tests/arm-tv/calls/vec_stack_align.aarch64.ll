
declare fastcc void @g1(double, <4 x i32>, double, <4 x i32>, double, <4 x i32>, double, <4 x i32>, double, <4 x i32>)

define void @f() {
  call void @g1(double 0.000000e+00,
                <4 x i32> zeroinitializer,
		double 0.000000e+00,
		<4 x i32> zeroinitializer,
		double 0.000000e+00,
		<4 x i32> zeroinitializer,
		double 0.000000e+00,
		<4 x i32> zeroinitializer,
		double 0.000000e+00,
		<4 x i32> <i32 1, i32 2, i32 3, i32 4>)
  ret void
}
