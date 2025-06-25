target datalayout = "e-m:e-p:64:64-i64:64-i128:128-n32:64-S128"
target triple = "riscv64"

%struct.tf_0_struct_3 = type { i16, %struct.tf_0_struct_2, i16, i16, i16, i8, i64, i16 }
%struct.tf_0_struct_2 = type { i32, i32, i8, %struct.tf_0_struct_1 }
%struct.tf_0_struct_1 = type { i64, i32, i32, i16, i16, i16 }

@tf_0_struct_obj_1 = external global %struct.tf_0_struct_3
@tf_0_var_74 = external global i64
@tf_0_array_1 = external global [2 x %struct.tf_0_struct_1]

define i16 @tf_0_foo(i16 %0, i32 %conv40, ptr %tf_0_var_44, i16 %1, i32 %2, i32 %conv179, i1 %3, i32 %4) #0 {
entry:
  %cmp12 = icmp eq i32 1, 1
  br i1 %cmp12, label %if.else, label %land.lhs.true

land.lhs.true:                                    ; preds = %entry
  %5 = load i16, ptr null, align 4
  %conv4 = sext i16 %5 to i32
  %shl = shl i32 1, %conv4
  %6 = load i16, ptr @tf_0_struct_obj_1, align 8
  %conv7 = sext i16 %6 to i32
  %add8 = or i32 %shl, %conv7
  %7 = load i64, ptr @tf_0_var_74, align 8
  %8 = load i32, ptr @tf_0_array_1, align 8
  %conv29 = sext i32 %8 to i64
  %and = and i64 %7, %conv29
  %sub41 = or i32 %conv40, -32488
  %add44 = or i32 %conv40, 2147483647
  %and46 = and i32 %add44, %sub41
  %mul47 = mul i32 %and46, %add8
  %conv48 = sext i32 %mul47 to i64
  %cmp49 = icmp ult i64 %and, %conv48
  br i1 %cmp49, label %if.then, label %if.else

if.then:                                          ; preds = %land.lhs.true
  %lnot.ext54 = zext i1 %3 to i16
  ret i16 %lnot.ext54

if.else:                                          ; preds = %land.lhs.true, %entry
  %tobool150.not = icmp eq i32 %2, %conv179
  br i1 %tobool150.not, label %if.end188, label %if.then156

if.then156:                                       ; preds = %if.else
  store i32 %4, ptr null, align 4
  br label %if.end188

if.end188:                                        ; preds = %if.then156, %if.else
  ret i16 0
}

attributes #0 = { "target-features"="+c" }
