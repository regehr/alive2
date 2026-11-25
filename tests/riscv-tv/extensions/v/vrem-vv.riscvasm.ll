; signed remainder: x % 0 = x, and INT_MIN % -1 = 0
define void @test(ptr %dst, ptr %pa, ptr %pb) {
entry:
  %a = load i8, ptr %pa, align 1
  %b = load i8, ptr %pb, align 1
  %z = icmp eq i8 %b, 0
  br i1 %z, label %done, label %notzero
notzero:
  %m1 = icmp eq i8 %b, -1
  br i1 %m1, label %done, label %rem
rem:
  %r0 = srem i8 %a, %b
  br label %done
done:
  %r = phi i8 [ %a, %entry ], [ 0, %notzero ], [ %r0, %rem ]
  store i8 %r, ptr %dst, align 1
  ret void
}
