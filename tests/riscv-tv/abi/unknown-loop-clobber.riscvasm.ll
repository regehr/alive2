; CHECK: 1 incorrect transformations
; TEST-ARGS: -src-unroll=3 -tgt-unroll=3

; Each dynamic execution of the call invalidation must produce a fresh value.
declare void @clobber()
define i64 @test() {
entry:
  br label %loop
loop:
  %i = phi i64 [0, %entry], [%next, %loop]
  call void @clobber()
  %next = add i64 %i, 1
  %done = icmp eq i64 %next, 2
  br i1 %done, label %exit, label %loop
exit:
  ret i64 0
}
