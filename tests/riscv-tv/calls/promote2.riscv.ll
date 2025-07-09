
define i32 @bar() {
  call void @foo(i64 noundef -1)
  call void @foo(i64 noundef -1)
  ret i32 -7
}

declare void @foo(i64 noundef) 
