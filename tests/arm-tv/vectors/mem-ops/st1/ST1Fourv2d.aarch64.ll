; Function Attrs: strictfp
define fastcc ptr @object_mouse_select_menu(ptr %buffer, i1 %0, ptr %mem) #0 {
entry:
  br label %vector.body

vector.body:                                      ; preds = %vector.body, %entry
  %vec.phi = phi <16 x i8> [ zeroinitializer, %entry ], [ %3, %vector.body ]
  %vec.phi69 = phi <16 x i8> [ zeroinitializer, %entry ], [ %4, %vector.body ]
  %wide.vec = load <64 x i32>, ptr %mem, align 4
  %wide.vec72 = load <64 x i32>, ptr %buffer, align 4
  %strided.vec = shufflevector <64 x i32> %wide.vec, <64 x i32> poison, <16 x i32> <i32 0, i32 4, i32 8, i32 12, i32 16, i32 20, i32 24, i32 28, i32 32, i32 36, i32 40, i32 44, i32 48, i32 52, i32 56, i32 60>
  %strided.vec75 = shufflevector <64 x i32> %wide.vec72, <64 x i32> poison, <16 x i32> <i32 0, i32 4, i32 8, i32 12, i32 16, i32 20, i32 24, i32 28, i32 32, i32 36, i32 40, i32 44, i32 48, i32 52, i32 56, i32 60>
  %1 = icmp eq <16 x i32> zeroinitializer, %strided.vec
  %2 = icmp eq <16 x i32> zeroinitializer, %strided.vec75
  %3 = select <16 x i1> %1, <16 x i8> zeroinitializer, <16 x i8> %vec.phi
  %4 = select <16 x i1> %2, <16 x i8> zeroinitializer, <16 x i8> %vec.phi69
  br i1 %0, label %vec.epilog.iter.check, label %vector.body

vec.epilog.iter.check:                            ; preds = %vector.body
  %rdx.select.cmp.not = icmp ne <16 x i8> %vec.phi, zeroinitializer
  %rdx.select.cmp78.not98 = icmp ne <16 x i8> %vec.phi69, zeroinitializer
  ret ptr null
}

attributes #0 = { strictfp }