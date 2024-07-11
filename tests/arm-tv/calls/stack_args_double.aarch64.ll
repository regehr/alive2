declare double @g(double, double, double, double, double, double, double, double, double, double, double, double, double, double, double)

define double @f(double %p0, double %p1, double %p2, double %p3, double %p4, double %p5, double %p6, double %p7, double %p8, double %p9, double %p10, double %p11, double %p12, double %p13, double %p14) {
  %x = call double @g(double %p14, double %p13, double %p12, double %p11, double %p10, double %p9, double %p8, double %p7, double %p6, double %p5, double %p4, double %p3, double %p2, double %p1, double %p0)
  ret double %x
}
