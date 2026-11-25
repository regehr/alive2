; an indexed segment load: field f of segment i is at base + off[i] + f
define void @test(ptr %f0, ptr %base, ptr %offs, ptr %f1) {
  %o0 = load i8, ptr %offs, align 1
  %q = getelementptr i8, ptr %offs, i64 1
  %o1 = load i8, ptr %q, align 1
  %w0 = zext i8 %o0 to i64
  %w1 = zext i8 %o1 to i64
  %s0 = getelementptr i8, ptr %base, i64 %w0
  %s1 = getelementptr i8, ptr %base, i64 %w1
  %x0 = load i8, ptr %s0, align 1
  %x1 = load i8, ptr %s1, align 1
  %t0 = getelementptr i8, ptr %s0, i64 1
  %t1 = getelementptr i8, ptr %s1, i64 1
  %y0 = load i8, ptr %t0, align 1
  %y1 = load i8, ptr %t1, align 1
  store i8 %x0, ptr %f0, align 1
  %u = getelementptr i8, ptr %f0, i64 1
  store i8 %x1, ptr %u, align 1
  store i8 %y0, ptr %f1, align 1
  %v = getelementptr i8, ptr %f1, i64 1
  store i8 %y1, ptr %v, align 1
  ret void
}
