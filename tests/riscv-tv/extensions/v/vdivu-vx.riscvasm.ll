; vector divides do not trap; dividing by zero gives all ones
define void @test(ptr %dst, ptr %src, i64 %x) {
entry:
  %a = load i8, ptr %src, align 1
  %b = trunc i64 %x to i8
  %z = icmp eq i8 %b, 0
  br i1 %z, label %done, label %div
div:
  %q = udiv i8 %a, %b
  br label %done
done:
  %r = phi i8 [ -1, %entry ], [ %q, %div ]
  store i8 %r, ptr %dst, align 1
  ret void
}
