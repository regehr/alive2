; vnmsub.vv vd, vs1, vs2: vd[i] = -(vs1[i] * vd[i]) + vs2[i]
define void @test(ptr %dst, ptr %pd, ptr %p1, ptr %p2) {
  %d = load i16, ptr %pd, align 2
  %s1 = load i16, ptr %p1, align 2
  %s2 = load i16, ptr %p2, align 2
  %m = mul i16 %s1, %d
  %r = sub i16 %s2, %m
  store i16 %r, ptr %dst, align 2
  ret void
}
