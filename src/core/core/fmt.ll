; ModuleID = 'fmt'
source_filename = "fmt"

@format = private unnamed_addr constant [4 x i8] c"%i\0A\00", align 1

declare i32 @printf(i8*, ...)

define {} @"core::fmt::print_int"(i32 %0) {
entry:
  %temp2 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @format, i32 0, i32 0), i32 %0)
  ret {} zeroinitializer
}
