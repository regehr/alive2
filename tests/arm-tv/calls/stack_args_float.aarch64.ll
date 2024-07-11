declare float @g(float, float, float, float, float, float, float, float, float, float, float, float, float, float, float)

define float @f(float %p0, float %p1, float %p2, float %p3, float %p4, float %p5, float %p6, float %p7, float %p8, float %p9, float %p10, float %p11, float %p12, float %p13, float %p14) {
  %x = call float @g(float %p14, float %p13, float %p12, float %p11, float %p10, float %p9, float %p8, float %p7, float %p6, float %p5, float %p4, float %p3, float %p2, float %p1, float %p0)
  ret float %x
}
