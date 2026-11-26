; CHECK: 1 correct transformations
; the address of a stack slot escapes to a callee. the lifter rebuilds it
; from SP with ptrtoint/add/inttoptr, which Alive2 won't match with the
; source's alloca unless the round trip is collapsed into a gep
declare void @f(ptr)

define void @t() {
  %a = alloca i32
  call void @f(ptr %a)
  ret void
}
