define void @foo(i1 %0, ptr %p) {
  %cond = or i1 %0, true
  br i1 %cond, label %end, label %then

then:
  store i64 0, ptr %p, align 8
  br label %end

end:
  ret void
}
