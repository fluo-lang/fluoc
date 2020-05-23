@format = private unnamed_addr constant [4 x i8] c"%i\0A\00", align 1

declare i32 @printf(i8*, ...)

define {} @print_i32(i32* %0) {
entry:
  %temp1 = load i32, i32* %0
  %temp2 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @format, i32 0, i32 0), i32 %temp1)
  ret {} zeroinitializer
}

define i32 @"tests2::hello"() {
entry:
  ret i32 10
}

define i32 @main() {
entry:
  %x = alloca i32
  store i32 123, i32* %x
  %print_i32 = call {} @print_i32(i32* %x)
  ret i32 0
}
