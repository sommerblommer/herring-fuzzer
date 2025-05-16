
declare void @print_integer(i32)
declare i32 @read_integer()
%struct.list = type { i32, [0 x i32] }
define i32 @main() {
	%call0 = call i32 @iinbdyt(i32 372, i32 951)
	%bop1 = sub i32 370, %call0
	%call2 = call i32 @iinbdyt(i32 733, i32 91)
	%bop3 = sdiv i32 %bop1, %call2
	call void @print_integer(i32 %bop3)
	ret i32 0

}
define i32 @iinbdyt(i32 %tgeopkrdkwqqofdremwmletrohlrt, i32 %lgyjjvzpeickjr, i32 %botew, i32%lzomkqoysglhqm) {
	%lzomkqoysglhqmptr0 = alloca i32
	%botewptr1 = alloca i32
	%lgyjjvzpeickjrptr2 = alloca i32
	%tgeopkrdkwqqofdremwmletrohlrtptr3 = alloca i32
	store i32 %lzomkqoysglhqm, ptr %lzomkqoysglhqmptr0
	store i32 %botew, ptr %botewptr1
	store i32 %lgyjjvzpeickjr, ptr %lgyjjvzpeickjrptr2
	store i32 %tgeopkrdkwqqofdremwmletrohlrt, ptr %tgeopkrdkwqqofdremwmletrohlrtptr3
	%load4 = load i32, ptr %lzomkqoysglhqmptr0
	ret i32 %load4

}
