define void @f2(ptr %a, ptr %b) {
  %v1 = load <16 x i8>, ptr %a
  %v2 = call <16 x i8> @llvm.ctpop.v16i8(<16 x i8> %v1)
  store <16 x i8> %v2, ptr %b
  ret void
}

declare <16 x i8> @llvm.ctpop.v16i8(<16 x i8>)