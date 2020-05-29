; ModuleID = 'op'
source_filename = "op"

define i32 @"core::op::add_int"(i32 %0, i32 %1) {
entry:
  %int_addition_temp = add i32 %0, %1
  ret i32 %int_addition_temp
}

define i32 @"core::op::mul_int"(i32 %0, i32 %1) {
entry:
  %int_multiplication_temp = mul i32 %0, %1
  ret i32 %int_multiplication_temp
}

define i32 @"core::op::sub_int"(i32 %0, i32 %1) {
entry:
  %int_subtraction_temp = sub i32 %0, %1
  ret i32 %int_subtraction_temp
}
