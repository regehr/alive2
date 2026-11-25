; vcompress packs the selected elements (here the nonzero bytes) at the
; start of vd; with tu, the rest keeps its old value, zero
define void @test(ptr %dst, ptr %src) {
entry:
  store i32 0, ptr %dst, align 1
  %a0 = load i8, ptr %src, align 1
  %p1 = getelementptr i8, ptr %src, i64 1
  %a1 = load i8, ptr %p1, align 1
  %p2 = getelementptr i8, ptr %src, i64 2
  %a2 = load i8, ptr %p2, align 1
  %p3 = getelementptr i8, ptr %src, i64 3
  %a3 = load i8, ptr %p3, align 1
  %n0 = icmp ne i8 %a0, 0
  br i1 %n0, label %s0, label %j0
s0:
  store i8 %a0, ptr %dst, align 1
  br label %j0
j0:
  %q1 = phi i64 [ 1, %s0 ], [ 0, %entry ]
  %n1 = icmp ne i8 %a1, 0
  br i1 %n1, label %s1, label %j1
s1:
  %w1 = getelementptr i8, ptr %dst, i64 %q1
  store i8 %a1, ptr %w1, align 1
  %q1n = add i64 %q1, 1
  br label %j1
j1:
  %q2 = phi i64 [ %q1n, %s1 ], [ %q1, %j0 ]
  %n2 = icmp ne i8 %a2, 0
  br i1 %n2, label %s2, label %j2
s2:
  %w2 = getelementptr i8, ptr %dst, i64 %q2
  store i8 %a2, ptr %w2, align 1
  %q2n = add i64 %q2, 1
  br label %j2
j2:
  %q3 = phi i64 [ %q2n, %s2 ], [ %q2, %j1 ]
  %n3 = icmp ne i8 %a3, 0
  br i1 %n3, label %s3, label %done
s3:
  %w3 = getelementptr i8, ptr %dst, i64 %q3
  store i8 %a3, ptr %w3, align 1
  br label %done
done:
  ret void
}
