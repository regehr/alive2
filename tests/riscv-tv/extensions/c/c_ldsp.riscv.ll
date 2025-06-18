target datalayout = "e-m:e-p:64:64-i64:64-i128:128-n32:64-S128"
target triple = "riscv64"

%struct.tf_0_struct_1 = type { i64, i64, i64, i64, i64 }

@tf_0_struct_obj_3 = external global %struct.tf_0_struct_1
@tf_0_var_4 = external constant i64

define i64 @tf_0_foo(ptr %tf_0_var_56, ptr %tf_0_var_76, ptr %tf_0_var_12, ptr %tf_0_var_52, ptr %0, i64 %1, i64 %cond, i64 %cond11) #0 {
entry:
  %tobool1.not = icmp eq i64 0, 0
  br i1 %tobool1.not, label %land.end, label %land.rhs

land.rhs:                                         ; preds = %entry
  %2 = load i64, ptr null, align 8
  %cmp = icmp slt i64 %2, 0
  %conv3 = zext i1 %cmp to i64
  %3 = load i64, ptr @tf_0_struct_obj_3, align 8
  %4 = load i64, ptr %tf_0_var_56, align 8
  %5 = load i64, ptr %tf_0_var_76, align 8
  %6 = load i64, ptr %tf_0_var_12, align 8
  %7 = load i64, ptr @tf_0_var_4, align 8
  %8 = load i64, ptr null, align 8
  %mul7 = mul i64 %3, 1600763522
  %mul6 = mul i64 %mul7, %cond
  %mul8 = mul i64 %mul6, %4
  %mul4 = mul i64 %mul8, %5
  %mul.neg = mul i64 %mul4, %6
  %mul5.neg = mul i64 %mul.neg, %7
  %mul9.neg = mul i64 %mul5.neg, %8
  %mul9.neg.masked = and i64 %mul9.neg, 4294967294
  %9 = or i64 %mul9.neg.masked, %conv3
  br label %land.end

land.end:                                         ; preds = %land.rhs, %entry
  %land.ext = phi i64 [ 0, %entry ], [ %9, %land.rhs ]
  %cmp14.not = icmp slt i64 %land.ext, 0
  br i1 %cmp14.not, label %cond.end, label %cond.true

cond.true:                                        ; preds = %land.end
  %10 = and i64 0, 4294967295
  br label %cond.end

cond.end:                                         ; preds = %cond.true, %land.end
  %cond111 = phi i64 [ %1, %cond.true ], [ 0, %land.end ]
  ret i64 %cond111
}

attributes #0 = { "target-features"="+c" }
