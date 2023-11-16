target triple = "wasm32-unknown-emscripten"

@g = external global [0 x i32], align 4

define i32 @load_test5(i32 %0) {
  %2 = getelementptr inbounds i32, ptr getelementptr inbounds ([0 x i32], ptr @g, i32 0, i32 10), i32 %0
  %3 = load i32, ptr %2, align 4
  ret i32 %3
}
