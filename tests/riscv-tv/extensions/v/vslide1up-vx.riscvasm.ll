; vslide1up.vx: vd[0] = the scalar, vd[i + 1] = vs2[i]
define void @test(ptr %p, i64 %x) {
  %s0 = load i8, ptr %p, align 1
  %p1 = getelementptr i8, ptr %p, i64 1
  %s1 = load i8, ptr %p1, align 1
  %p2 = getelementptr i8, ptr %p, i64 2
  %s2 = load i8, ptr %p2, align 1
  %t = trunc i64 %x to i8
  store i8 %t, ptr %p, align 1
  store i8 %s0, ptr %p1, align 1
  store i8 %s1, ptr %p2, align 1
  ret void
}
