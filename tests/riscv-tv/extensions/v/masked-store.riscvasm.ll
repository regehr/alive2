; bits 0 and 1 of %m are the mask; only active elements are stored
define void @test(ptr %dst, i64 %m) {
entry:
  %b0 = and i64 %m, 1
  %a0 = icmp ne i64 %b0, 0
  br i1 %a0, label %st0, label %next
st0:
  store i8 7, ptr %dst
  br label %next
next:
  %b1 = and i64 %m, 2
  %a1 = icmp ne i64 %b1, 0
  br i1 %a1, label %st1, label %done
st1:
  %d1 = getelementptr i8, ptr %dst, i64 1
  store i8 7, ptr %d1
  br label %done
done:
  ret void
}
