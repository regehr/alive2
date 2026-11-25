; CHECK: 1 incorrect transformations
; an unordered indexed store with two equal offsets leaves either value in
; memory; the lifter conservatively treats overlapping active elements of
; an unordered store as a fault, so this does not verify
define void @test(ptr %base, ptr %data, ptr %offs) {
  %v0 = load i32, ptr %data, align 4
  %r1 = getelementptr i8, ptr %data, i64 4
  %v1 = load i32, ptr %r1, align 4
  %o0 = load i32, ptr %offs, align 4
  %q1 = getelementptr i8, ptr %offs, i64 4
  %o1 = load i32, ptr %q1, align 4
  %w0 = zext i32 %o0 to i64
  %w1 = zext i32 %o1 to i64
  %p0 = getelementptr i8, ptr %base, i64 %w0
  %p1 = getelementptr i8, ptr %base, i64 %w1
  store i32 %v0, ptr %p0, align 4
  store i32 %v1, ptr %p1, align 4
  ret void
}
