#  head.s contains the 32-bit startup code.
#  Two L3 task multitasking. The code of tasks are in kernel area, 
#  just like the Linux. The kernel code is located at 0x10000. 
# 原作者linus，注释作者:sp00f
# 下面的汇编是AT&T格式的汇编，和原生汇编存在一定差异
# AT&T风格的汇编源操作数在左边，目的操作数在右边，和原生汇编正好相反
# 此时boot代码已经走完，进入了内核启动阶段，此段代码就是一个小的内核启动代码
# 内核是运行在保护模式下，即寻址方式是段选择符 + 偏移方式
# 程序刚开始依赖boot.s创建的GDT

SCRN_SEL	= 0x18
TSS0_SEL	= 0x20
LDT0_SEL	= 0x28
TSS1_SEL	= 0X30
LDT1_SEL	= 0x38

.text
startup_32:
	movl $0x10,%eax
	mov %ax,%ds			# 段寄存器ds装入0x10, 0000 0000 0001 0000, TI = 0, CPL = 0(内核态)， DI = 2, 是GDT第二个段，即内核数据段，见boot.s的GDT
#	mov %ax,%es			# 重新初始化内核栈
	lss init_stack,%esp # 把init_stack指向的内存地址的前四字节装入ESP寄存器，后两字节装入SS段寄存器，
						# 结果是把init_stack地址赋值给esp寄存器，0x10装入ss段寄存器，同样指向内核数据段，但偏移为esp

# 装载IDT和GDT
# setup base fields of descriptors.
	call setup_idt		# 装载中断向量表
	call setup_gdt		# 重新装载全局段描述符表
	movl $0x10,%eax		# reload all the segment registers # 因为重新初始化了GDT，因此重新设置各个段寄存器对应的段选择符，0x10如上还是指向GDT第二个段，数据段
	mov %ax,%ds		    # after changing gdt. 
	mov %ax,%es		    # 同上
	mov %ax,%fs
	mov %ax,%gs
	lss init_stack,%esp # 同上，重新设置内核栈

# 设置8253芯片，用于时钟，频率100hz
# 8253是Intel系列的计数/定时器电路，也称可编程间隔定时器，PIT(Programmable Interval Timer)
# 提供三个相同的计数器通道，每个通道都包含：
# 1个8位控制字寄存器
# 1个16位计数初值寄存器
# 1个计数执行部件
# 1个输出锁存器
# 其他可查看该芯片使用手册了解详细信息
# 8253控制字格式(8位数据线)
#    d7  d6   d5  d4  d3  d2  d1  d0
#   +---+---+---+---+---+---+---+---+
#   |sc1|sc0|rw1|rw0|m2 |m1 |m0 |bcd|
#   +---+---+---+---+---+---+---+---+
#     计数器  读写    工作方式   十/二进制

# setup up timer 8253 chip.
	movb $0x36, %al			# 0x36即 0011 0110,  选择通道0，先读/写低字节，工作方式3（方波发生器），二进制计数
	movl $0x43, %edx		# 指向控制口，0x43控制端口(这里8253端口为40h-43h)
	outb %al, %dx			# 写入控制字
	movl $11930, %eax       # timer frequency 100 HZ # CLK脉冲的分频系数，单位时间100次/s
	movl $0x40, %edx		# 通道0端口
	outb %al, %dx			# 写入低8字节
	movb %ah, %al
	outb %al, %dx			# 写入高8字节

# 第二次初始化idt表
# 安装时钟中断、系统调用中断
# setup timer & system call interrupt descriptors.
	movl $0x00080000, %eax			# 代码类似第一次初始化idt，cs = 0008
	movw $timer_interrupt, %ax		# timer_interrupt低16位偏移
	movw $0x8E00, %dx				# 中断门
	movl $0x08, %ecx              # The PC default timer int. # pc默认int $0x8是调用timer吧
	lea idt(,%ecx,8), %esi			# 8*ecx + idt， ecx = 8 ， 即idt + 64，是取idt[8]表项地址，对应int $0x8，通常情况下idt[0]是不用的
	movl %eax,(%esi) 				# 设置idt表项低4字节
	movl %edx,4(%esi)				# 设置idt表项高4字节
	movw $system_interrupt, %ax		# system_interrupt低16位偏移
	movw $0xef00, %dx				# 0xef00对应16位二进制为 1110 1111 0000 0000，根据深入理解linux内核，它对应陷阱门描述符
	movl $0x80, %ecx				# 对应 int $ 0x80
	lea idt(,%ecx,8), %esi			# 取idt[80]项，对应 int $ 0x80
	movl %eax,(%esi) 				# 设置idt表项低4字节
	movl %edx,4(%esi)				# 设置idt表项高4字节

# unmask the timer interrupt.
#	movl $0x21, %edx
#	inb %dx, %al
#	andb $0xfe, %al
#	outb %al, %dx

# Move to user mode (task 0)
	pushfl						# 压入标志寄存器
	andl $0xffffbfff, (%esp)	# 设置栈，掩码第15位，这里干嘛我也没弄明白
	popfl						# 弹出标志寄存器
	movl $TSS0_SEL, %eax
	ltr %ax						# 装载TSS0_SEL	= 0x20 到任务寄存器的段选择器字段， 0x20为0000 0000 0010 0000，DI = 4，即TSS0，理论上这里会加载SS和ESP(都为0)
	movl $LDT0_SEL, %eax		# LDT0_SEL	= 0x28
	lldt %ax 					# 装载LDT0_SEL	= 0x28到ldtr寄存器， 0x28为0000 0000 0010 1000，DI = 5， 即LDT0
	movl $0, current			# 设置current为0
	sti							# 开启中断，允许中断发生，通常的，iret指令会从堆栈弹出代码段选择符到CS寄存器，同时会弹出偏移到IP，还会弹出标志寄存器
	pushl $0x17					# 数据段对应的段选择符，0x17对应16位二进制为0000 0000 0001 0111, TI = 1, DPL = 3(用户态), DI = 2, 到LTD0的第二个段
	pushl $init_stack			# 此处涉及不同保护级别切换，iret还会弹出堆栈信息，包括堆栈段选择符到SS段寄存器，和esp，形成 SS:init_stack
	pushfl						# 压入标志寄存器， iret会弹出
	pushl $0x0f					# 0x0f对应16位二进制为0000 0000 0000 1111, TI = 1, DPL = 3(用户态), DI = 1, 到LTD0的第一个段，代码段
	pushl $task0                # 压入task0偏移，iret会弹出形成CS:IP，这里为 0:task0
	iret						# iret后跳转到0:task0处执行逻辑（这是linux的独特之处，它内核态和用户态代码段、数据段都是从地址0开始，这样偏移就直接对应线性地址）

/****************************************/
setup_gdt:
	lgdt lgdt_opcode	# 重新初始化全局描述符表，共8个GDT表项，其中后四项指向LDT(ldt0, ldt1)和TSS(tss0, tss1)描述符
	ret


# 在真正的Linux内核源码中，ignore_int中断例程可以被认为是一个空的例程，它不会被调用，除非发生错误，第一次初始化所有中断表项都被填充为ignore_int
# 在第二次初始化时，它会被重新填充，填充为真正的idt表项
# 保护模式分段寻址是段选择符 ：偏移， 0008对应二进制的16位为0000 0000 0000 1000，TI = 0，是GDT中的段，cpl = 0 （内核态）
# DI = 1，是代码GDT的第一个段，即内核代码段，从boot.s的GDT中我们可以看到内核代码的base为0x0000
# 此次填充256个默认的中断例程ignore_int到idt其实的地址处。
setup_idt:
	lea ignore_int,%edx			# 取ignore_int的地址，ignore_int是在内核代码段，0x0008代表内核代码段，dx是ignore_int段内低16bit偏移
	movl $0x00080000,%eax		# 执行完后eax变为 0008 dx(ignore_int低16位偏移)，edx高16位为ignore_int高16位偏移，低16位为中断描述符表项的32-47位
	movw %dx,%ax		/* selector = 0x0008 = cs */  #这么做的原因完全是中断描述符的规则，它64bit需要两个寄存器才能容纳，见下图
	movw $0x8E00,%dx	/* interrupt gate - dpl=0, present */ # 0x8e00对应16位二进制数为1000 1110 0000 0000，根据深入理解linux内核，
                        # 此中断描述符代表中断门描述符（另外两种为任务门和陷阱门），中断门描述符如下:
                        # 63                            48 47   45 44 43 42 41 40 39 38 37              32
                        # +-------------------------------+--+----+--+--+--+--+--+--+--+--+--------------+
                        # |偏移量(16-31)                  |p | dp |0 |1 |1 |1 |0 |0 |0 |0 | 保留         |
                        # +------------------------------------------------------------------------------+
                        # |段选择符                       |偏移量(0-15)                                  |
                        # +-------------------------------+----------------------------------------------+
                        # 31                             16                                              0
                        # 由此可以得知此中断描述符 dpl = 0， present = 1，也就是英文注释的意思
	lea idt,%edi			# 取idt内存地址，idt为256*8字节的0区域
	mov $256,%ecx			# 循环256次，idt共256个条目
rp_sidt:					# 都被填充为一样的ignore_int
	movl %eax,(%edi)		# idt一个表项的低4字节，见中断门描述符，eax的ax为ignore_int的低16位偏移，高16bit为段选择符即0008
	movl %edx,4(%edi)		# idt一个表项的高4字节，见中断门描述符，edx的dx为0x8E00，如上描述，高16bit为ignore_int的高16偏移
	addl $8,%edi			# idt索引加1
	dec %ecx
	jne rp_sidt
	lidt lidt_opcode		# 装载.idt地址和表项数量到idtr寄存器
	ret

# -----------------------------------
write_char:
	push %gs				# 保存一些寄存器信息
	pushl %ebx
#	pushl %eax
	mov $SCRN_SEL, %ebx		# SCRN_SEL	= 0x18对应0000 0000 0001 1000,TI = 0(对应GDT), CPL=0(内核态), DI = 3, 对应screen 0x18 - for display
	mov %bx, %gs			# gs段寄存器为0x18
	movl scr_loc, %bx		# 把scr_loc对应值，即屏幕位置赋值给bx
	shl $1, %ebx			# 通过搜索了解到显示一个字符要用两个字节，低字节是字符，高字节是显示属性（如颜色、背景等），逻辑左移一位等于乘2
                            # 屏幕位置和显存位置相差两倍关系，如下：
                            # 0 0:0,1
                            # 1 2:2,3
                            # 2 4:4,5
                            # 3 6:6,7
	movb %al, %gs:(%ebx)	# 输出al对应的ascii字符到显示器，从显示器第0个位置开始
	shr $1, %ebx			# 在逻辑右移1，除以2变回原值
	incl %ebx				# ebx加1，屏幕位置加1
	cmpl $2000, %ebx		# 和2000对比
	jb 1f
	movl $0, %ebx			# 输出超过2000个字符后，在从头开始输出
1:	movl %ebx, scr_loc		# 把ebx的值存入scr_loc
#	popl %eax				# 恢复寄存器信息
	popl %ebx
	pop %gs
	ret						# 退出函数

/***********************************************/
/* This is the default interrupt "handler" :-) */
.align 2
ignore_int:				# 在真正的Linux内核源码中，只是一个默认的中断例程
	push %ds			# 这可以理解为它是一个假的占位中断例程，它几乎不会被执行，执行了肯定是发生了错误
	pushl %eax			
	movl $0x10, %eax
	mov %ax, %ds
	movl $67, %eax            /* print 'C' */
	call write_char
	popl %eax
	pop %ds
	iret

# 时间中断，对应int $0x8
/* Timer interrupt handler */ 
.align 2
timer_interrupt:
	push %ds				# 这里中断用到了8259中断控制器，细节需要查看相关手册
	pushl %eax				# 保存寄存器
	movl $0x10, %eax		# 0000 0000 0001 0000，内核态第二个段，即内核数据段，linux没有使用TSS记录ds，在进程切换（特别是跨级别）是需要重新设置ds
	mov %ax, %ds			# 否则它找不到内核相关数据，这里current就在内核数据段，这里每次都是重新设置ds，我的理解是这样的。
	movb $0x20, %al			# 和下面一行的作用是发送EOI结束中断指令，通知8259A芯片一个中断完成，8259a将负责把ISR中的位清除，以便以后可以继续接受中断。
	outb %al, $0x20			# 20H-3FH可编程中断控制器1(8259)使用
							# 如果不加的话，8259a永远收不到中断结束命令，那么就认为某一个中断一直在执行，
							# 所以如果遇到比这个阻塞的中断级别低或者相等的中断发生时就不会再响应了（以上关于8259芯片内容摘自网络）。
	movl $1, %eax
	cmpl %eax, current		# current和1比较
	je 1f
	movl %eax, current		# current != 1，将其赋值为1
	ljmp $TSS1_SEL, $0		# long jump，TSS1_SEL = 0X30即0000 0000 0011 0000，对应TSS1 descr段，又TSS1->tss1(eip指向task1) -> task1
	jmp 2f					# 跳转到lable 2处，感觉永远都走不到label 2处，我应该可以确定就是执行不到
1:	movl $0, current		# current = 1跳转到此
	ljmp $TSS0_SEL, $0		# TSS0 descr，指向我还在想，tss0记录的eip是0它怎么实现切换的呢，经过思考每次任务切换cpu会更新tss，eip就会被写进去，它就实现了任务切换
2:	popl %eax				# 恢复寄存器				# 这是接上面，但实际的linux操作系统好像没用tss中的eip，它被保留到栈上了，不过这里还不是linux，它目前遵守硬件
	pop %ds												# 约定，之所以这样，我思考了，这里根本就不可能执行到iret，如果能执行到iret是需要在栈上弹出ip的。
	iret					# 结束中断

# 陷阱门，对应系统调用 int $0x80，每次内核态和用户态之间的切换，都会通过TSS读取ss0、esp0、esp、ss到对应的寄存器，完成内核态和用户态堆栈的切换（其他linux未用）
/* system call handler */
.align 2
system_interrupt:
	push %ds			# 保存一些寄存器信息
	pushl %edx
	pushl %ecx
	pushl %ebx
	pushl %eax
	movl $0x10, %edx
	mov %dx, %ds		# 0x10为0000 0000 0001 0000，TI = 0(对应GDT), CPL = 0(内核态), DI = 2, 对应内核数据段
	call write_char		# 调用write_char
	popl %eax
	popl %ebx
	popl %ecx
	popl %edx
	pop %ds				# 恢复寄存器信息
	iret				# 恢复到中断之前的下一条指令处

/*********************************************/
current:.long 0
scr_loc:.long 0

.align 2
lidt_opcode:
	.word 256*8-1		# idt contains 256 entries 		# idt大小
	.long idt		# This will be rewrite by code.		# idt基址 
lgdt_opcode:		# 操作数是32位
	.word (end_gdt-gdt)-1	# so does gdt, GDT占用字节数
	.long gdt		# This will be rewrite by code. GDT基址

	.align 3
idt:	.fill 256,8,0		# idt is uninitialized # 共256个条目，每个条目8个字节，这里是占位

gdt:	.quad 0x0000000000000000	/* NULL descriptor */
	.quad 0x00c09a00000007ff	/* 8Mb 0x08, base = 0x00000 */
	.quad 0x00c09200000007ff	/* 8Mb 0x10 */
	.quad 0x00c0920b80000002	/* screen 0x18 - for display */	# base = 0xb80000, 8k

	.word 0x0068, tss0, 0xe900, 0x0	# TSS0 descr 0x20
	.word 0x0040, ldt0, 0xe200, 0x0	# LDT0 descr 0x28
	.word 0x0068, tss1, 0xe900, 0x0	# TSS1 descr 0x30
	.word 0x0040, ldt1, 0xe200, 0x0	# LDT1 descr 0x38
end_gdt:
	.fill 128,4,0
init_stack:                          # Will be used as user stack for task0.
	.long init_stack
	.word 0x10

/*************************************/
.align 3
ldt0:	.quad 0x0000000000000000
	.quad 0x00c0fa00000003ff	# 0x0f, base = 0x00000
	.quad 0x00c0f200000003ff	# 0x17

tss0:	.long 0 			/* back link */
	.long krn_stk0, 0x10		/* esp0, ss0 */
	.long 0, 0, 0, 0, 0		/* esp1, ss1, esp2, ss2, cr3 */
	.long 0, 0, 0, 0, 0		/* eip, eflags, eax, ecx, edx */
	.long 0, 0, 0, 0, 0		/* ebx esp, ebp, esi, edi */
	.long 0, 0, 0, 0, 0, 0 		/* es, cs, ss, ds, fs, gs */
	.long LDT0_SEL, 0x8000000	/* ldt, trace bitmap */

	.fill 128,4,0
krn_stk0:
#	.long 0

/************************************/
.align 3
ldt1:	.quad 0x0000000000000000
	.quad 0x00c0fa00000003ff	# 0x0f, base = 0x00000
	.quad 0x00c0f200000003ff	# 0x17

tss1:	.long 0 			/* back link */
	.long krn_stk1, 0x10		/* esp0, ss0 */
	.long 0, 0, 0, 0, 0		/* esp1, ss1, esp2, ss2, cr3 */
	.long task1, 0x200		/* eip, eflags */
	.long 0, 0, 0, 0		/* eax, ecx, edx, ebx */
	.long usr_stk1, 0, 0, 0		/* esp, ebp, esi, edi */
	.long 0x17,0x0f,0x17,0x17,0x17,0x17 /* es, cs, ss, ds, fs, gs */
	.long LDT1_SEL, 0x8000000	/* ldt, trace bitmap */

	.fill 128,4,0
krn_stk1:

/************************************/
task0:
	movl $0x17, %eax			# 打印A字符
	movw %ax, %ds				# 0x17对应ldt0的第二项，是task0的数据段
	movl $65, %al              /* print 'A' */ # 打印字符'A'
	int $0x80					# 调用中断0x80， 进入内核system_interrupt调用write_char
	movl $0xfff, %ecx			# 设置计数器(ecx通常用作通用计数器)
1:	loop 1b						# 每次ecx - 1 ，如果ecx不为0一直循环
	jmp task0 					# 继续跳转到task0， 你会以为程序就这样，但其实不是，因为还有一个时钟呢，它会触发int $0x8中断system_interrupt
								# 它以每100次/s的频率触发时间中断

task1:
	movl $0x17, %eax			# 基本和task0相同，只不过打印B字符, 它有自己的局部段对应ldt1和tss段对应tss1
	movw %ax, %ds
	movl $66, %al              /* print 'B' */
	int $0x80
	movl $0xfff, %ecx
1:	loop 1b
	jmp task1

	.fill 128,4,0 
usr_stk1:
