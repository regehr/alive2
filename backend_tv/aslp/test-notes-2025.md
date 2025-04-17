```
found 16 .log files in logs
logs/fptoui.aarch64.ll.log|[c] Translation validated as correct|
logs/global-test-51.aarch64.ll.log|[f] ERROR: Source is more defined than target|
logs/LD1Fourv2d.aarch64.ll.log|[f] ERROR: Source is more defined than target|
logs/offset-test-10.aarch64.ll.log|[f] ERROR: Source is more defined than target|
logs/offset-test-13.aarch64.ll.log|[f] ERROR: Source is more defined than target|
logs/offset-test-4.aarch64.ll.log|[f] ERROR: Source is more defined than target|
logs/offset-test-47.aarch64.ll.log|[f] ERROR: Source is more defined than target|
logs/offset-test-55.aarch64.ll.log|[f] ERROR: Source is more defined than target|
logs/offset-test-78.aarch64.ll.log|[f] ERROR: Source is more defined than target|
logs/ST1Fourv2d.aarch64.ll.log|[f] ERROR: Source is more defined than target|
logs/t1.aarch64.ll.log|[f] ERROR: Source is more defined than target|
logs/t1b.aarch64.ll.log|[f] ERROR: Source is more defined than target|
logs/t3.aarch64.ll.log|[f] ERROR: Source is more defined than target|
logs/t4.aarch64.ll.log|[f] ERROR: Source is more defined than target|
logs/TBLv16i8Two.aarch64.ll.log|[c] Translation validated as correct|
logs/vec_stack_align_2.aarch64.ll.log|[f] ERROR: Source is more defined than target|
```

## 2025-04-17

all of the "source is more defined" errors appear to be memory-related.
for example:

- global-test-51.aarch64.ll.log: unknown UB at store - memory model issue? classic too

the simplest example is probably this one:
```
../build/backend-tv t1.aarch64.ll
```

```
----------------------------------------
declare void @g()

define i32 @f(i32 noundef %#0) {
entry:
  tail call void @g()
  %mul = shl nsw i32 noundef %#0, 1
  ret i32 %mul
}
=>
declare void @g()

define i32 @f(i32 noundef %#0) noundef asm {
arm_tv_entry:
  %stack24 = alloca i64 1 x i64 1280, align 16
  %#1 = gep inbounds nuw ptr %stack24, 1 x i64 1008
  %#2 = gep inbounds nuw ptr %stack24, 1 x i64 1016
  tail memset ptr %#1 align 16, i8 0, i64 16
  tail call void @g()
  %a7_4 = shl i32 noundef %#0, 1
  %a8_5 = load i64, ptr %#2, align 8
  %a9_4 = icmp eq i64 %a8_5, 0
  assume i1 %a9_4
  %#range_0_%a7_4 = !range i32 %a7_4, i32 0, i32 4294967295
  ret i32 %#range_0_%a7_4
}
```

```
}
Transformation doesn't verify!

ERROR: Source is more defined than target

Example:
i32 noundef %#0 = #x00000003 (3)

Source:
void = function did not return!

SOURCE MEMORY STATE
===================
NON-LOCAL BLOCKS:
Block 0 >       size: 0 align: 8        alloc type: 0   alive: false    address: 0
Block 1 >       size: 2178      align: 64       alloc type: 0   alive: true     address: 2737061436142052480
Contents:
*: #x0000000000000000


Target:
ptr %stack24 = pointer(local, block_id=0, offset=0) / Address=#x8000000000000000
ptr %#1 = pointer(local, block_id=0, offset=1008) / Address=#x80000000000003f0
ptr %#2 = pointer(local, block_id=0, offset=1016) / Address=#x80000000000003f8
void = UB triggered!

TARGET MEMORY STATE
===================
LOCAL BLOCKS:
Block 2 >       size: 1280      align: 16       alloc type: 1   alive: true     address: 9223372036854775808
```

the UB is triggered at the memset line which zeros stack values? this matches the other failing tests.
it's not clear why this is failing, as the pointers should be inbounds.

most of these failures are shared with the hand-written lifter too.
