; bits 0 and 1 of %m are the mask. an active element is loaded, an
; inactive one keeps its old value (mu) and its address is not accessed
define void @test(ptr %dst, ptr %src, i64 %m) {
entry:
  %b0 = and i64 %m, 1
  %a0 = icmp ne i64 %b0, 0
  br i1 %a0, label %ld0, label %next
ld0:
  %x0 = load i32, ptr %src, align 4
  br label %next
next:
  %v0 = phi i32 [ %x0, %ld0 ], [ 5, %entry ]
  %b1 = and i64 %m, 2
  %a1 = icmp ne i64 %b1, 0
  br i1 %a1, label %ld1, label %done
ld1:
  %s1 = getelementptr i8, ptr %src, i64 4
  %x1 = load i32, ptr %s1, align 4
  br label %done
done:
  %v1 = phi i32 [ %x1, %ld1 ], [ 5, %next ]
  store i32 %v0, ptr %dst, align 4
  %d1 = getelementptr i8, ptr %dst, i64 4
  store i32 %v1, ptr %d1, align 4
  ret void
}
