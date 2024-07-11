define void @f() {
  call void asm sideeffect "", "~{d8},~{d9}"()
  call void @bar()
  ret void
}

declare void @bar()
