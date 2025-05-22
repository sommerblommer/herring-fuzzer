
declare void @print_integer(i32)
declare i32 @read_integer()
%struct.list = type { i32, [0 x i32] }
define i32 @main() {
	call void @print_integer(i32 973)
	ret i32 0

}
define i32 @x(i32 %uwjn, i32 %cq, i32 %woji, i32%p) {
	%pptr0 = alloca i32
	%wojiptr1 = alloca i32
	%cqptr2 = alloca i32
	%uwjnptr3 = alloca i32
	store i32 %p, ptr %pptr0
	store i32 %woji, ptr %wojiptr1
	store i32 %cq, ptr %cqptr2
	store i32 %uwjn, ptr %uwjnptr3
	%load4 = load i32, ptr %wojiptr1
	%bop5 = mul i32 151, %load4
	ret i32 %bop5

}
