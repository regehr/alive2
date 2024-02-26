; Function Attrs: strictfp
define i32 @BKE_pbvh_count_grid_quads(ptr %grid_hidden, ptr %grid_indices, i1 %0) #0 {
entry:
  br label %vector.body

vector.body:                                      ; preds = %vector.body, %entry
  %index1 = phi i64 [ 0, %entry ], [ %index.next, %vector.body ]
  %vec.phi37 = phi i32 [ 0, %entry ], [ %16, %vector.body ]
  %vec.phi38 = phi i32 [ 0, %entry ], [ %17, %vector.body ]
  %1 = or disjoint i64 %index1, 1
  %2 = getelementptr i32, ptr %grid_indices, i64 %index1
  %3 = getelementptr i32, ptr %grid_indices, i64 %index1
  %4 = getelementptr i32, ptr %grid_indices, i64 %1
  %5 = load i32, ptr %2, align 4
  %6 = load i32, ptr %3, align 4
  %7 = load i32, ptr %4, align 4
  %8 = sext i32 %6 to i64
  %9 = sext i32 %7 to i64
  %10 = getelementptr ptr, ptr %grid_hidden, i64 %8
  %11 = getelementptr ptr, ptr %grid_hidden, i64 %9
  %12 = icmp eq ptr %10, null
  %13 = icmp eq ptr %11, null
  %14 = select i1 %12, i32 1, i32 0
  %15 = select i1 %13, i32 1, i32 0
  %16 = or i32 %vec.phi37, %14
  %17 = or i32 %vec.phi38, %15
  %index.next = add i64 %index1, 4
  br i1 %0, label %middle.block, label %vector.body

middle.block:                                     ; preds = %vector.body
  %bin.rdx40 = or i32 %vec.phi38, %vec.phi37
  ret i32 0
}

attributes #0 = { strictfp }