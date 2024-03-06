define i1 @f(<2 x i64> %0) {
  %2 = and <2 x i64> %0, <i64 4, i64 4>
  %3 = icmp ne <2 x i64> %2, zeroinitializer
  %4 = bitcast <2 x i1> %3 to i2
  %5 = icmp eq i2 %4, -1
  ret i1 %5
}