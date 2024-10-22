declare ptr @g(ptr, ptr, ptr, ptr, ptr, ptr, ptr, ptr, ptr, ptr, ptr, ptr, ptr, ptr, ptr)

define ptr @f(ptr %p0, ptr %p1, ptr %p2, ptr %p3, ptr %p4, ptr %p5, ptr %p6, ptr %p7, ptr %p8, ptr %p9, ptr %p10, ptr %p11, ptr %p12, ptr %p13, ptr %p14) {
  %x = call ptr @g(ptr %p14, ptr %p13, ptr %p12, ptr %p11, ptr %p10, ptr %p9, ptr %p8, ptr %p7, ptr %p6, ptr %p5, ptr %p4, ptr %p3, ptr %p2, ptr %p1, ptr %p0)
  ret ptr %x
}
