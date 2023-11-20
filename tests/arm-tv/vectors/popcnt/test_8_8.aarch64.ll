define void @f1(ptr %a, ptr %b) {
  %v1 = load <8 x i8>, ptr %a
  %v2 = call <8 x i8> @llvm.ctpop.v8i8(<8 x i8> %v1)
  store <8 x i8> %v2, ptr %b
  ret void
}

declare <8 x i8> @llvm.ctpop.v8i8(<8 x i8>)