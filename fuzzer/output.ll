
declare void @print_integer(i32)
declare i32 @read_integer()
%struct.list = type { i32, [0 x i32] }
define i32 @main() {
	call void @print_integer(i32 24567)
	ret i32 0

}
