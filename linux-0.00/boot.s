!	boot.s
!
! It then loads the system at 0x10000, using BIOS interrupts. Thereafter
! it disables all interrupts, changes to protected mode, and calls the 
! 原作者linus，注释作者:sp00f
! 实模式分段模式
! 这个程序之所以这样是和历史有关，早期8080和兼容的8086，程序被限制在64k字节(65535)的段内运行，也是段的最大长度。
! 64k是2^16次方，是因为8086cpu的数据总线是16位，因此最多取64k数据。
! 段是从段落边界开始的，每个段落16字节大小，也就是说两个段至少相隔16字节。
! 实模式下和保护模式下，cpu对分段寻址方式有所不同，实模式寻址方式是：段基址 + 偏移，保护模式是：段选择符 + 偏移
! 之所以这样是因为当年在一定历史时间内8086和其向后兼容的cpu实模式下可寻址2^20即1m大小空间（有20根地址线），而
! 最大的数据寻址范围为64k，所以才设计这个分段寻址方式。
! 8086和其向后兼容的cpu的段寄存器是16位宽（向后兼容的cpu也是把段寄存器限制在16位），因此实模式下要用两个16位寄存器来寻址。
! 实模式下寻址示例：jmpi	go,#BOOTSEG， BOOTSEG被赋值给段寄存器cs，go代表偏移
! 很明显实模式下cpu不需要GDT、LDT、页表这些，只有保护模式下才需要，并且在使用之前需要对它们先进行初始化。
!
! 保护模式下，分段寻址需要用到GDT和LDT，示例：jmpi 0,8, 跳转到GDT指向的第1个段偏移0处，为什么是第一个段，看段选择符格式
! 段选择符格式是：
! 15                       3  2  1  0
! +-------------------------+---+---+  TI = 0标识GDT，TI = 1标识LDT
! |                         |TI |RPL|  RPL请求者特权级，用两个位表示，分别为 00,01,10,11,即0、1、2、3个特权级，0最高
! +-------------------------+---+---+
! Linux 只用 0(内核态)和3(用户态)

! GDT中每个表项占8个字节，即64位，寄存器gdtr保存着指向GDT列表的基址
! gdtr里面装载着GDT的基址和边界大小
! 
! 0x1000 * 0x10 = 0x10000 即 65536，即下面的程序被加载到第0x1000个段落所在的段中，地址是0x10000

BOOTSEG = 0x07c0			!
SYSSEG  = 0x1000			! system loaded at 0x10000 (65536). 
SYSLEN  = 17				! sectors occupied.

entry start
start:						! 引导程序
	jmpi	go,#BOOTSEG  	! 段间跳转，实模式分段模式下， 此指令完成将0x07c0赋值给cs寄存器，并跳转到cs:go处，段：偏移是分段模式的寻找方式
go:	mov	ax,cs				! 设置段寄存器为0x07c0，和BIOS引导设备有关，引导签名位于引导扇区（扇区号0）中，一个好的做法是在启动扇区的一开始就强制执行CS:IP
	mov	ds,ax				! 设置数据段
	mov	ss,ax  				! 设置堆栈 ss:sp = 0x07c0:0x400
	mov	sp,#0x400			! arbitrary value >>512


! 此例程完成通过从软盘0磁头0柱面的第二个扇区开启的连续17个扇区的内容到段0x1000:0处，即内存地址0x10000处
! ok, we've written the message, now
load_system:
	mov	dx,#0x0000  		! DL Drive 0 DH Head（磁头）， 根据wike手册 DL = 00h， DH = 0h	1st floppy disk ( "drive A:" )，这里是从软盘引导，磁头是0
	mov	cx,#0x0002  		! CL Sector , CH Cylinder， sector 2, cylinder 0 , 0柱2扇区
	mov	ax,#SYSSEG
	mov	es,ax  				! 内存段起始地址 ES
	xor	bx,bx  				! 内存偏移  最终ES:bx = 0x1000:0，即内存地址0x10000处
	mov	ax,#0x200+SYSLEN  	! 0x211, AH = 0010, 02h是Read Sectors From Drive， AL = 10001, Sectors To Read Count, 0x11等于17次，和上面所占用的扇区数相等
	int 	0x13					! 此BIOS中断服务是磁盘服务例程
	jnc	ok_load				! 如果出现错误CF会被设置，CF	Set on error，因此检测CF进位标记
die:	jmp	die


! now we want to move to protected mode ...
ok_load:
	cli			! no interrupts allowed ! 禁止中断
	mov	ax, #SYSSEG	! si,di 它们只能用做16位寄存器，一般用来存放地址。在串处理指令中，
	mov	ds, ax		! si用作隐含的源串地址，默认在DS中；di用做隐含的目的串地址，默认在ES中。 分别达到在数据段和附加段中寻址的目的.
	xor	ax, ax
	mov	es, ax
	mov	cx, #0x2000 ! 循环0x2000次
	sub	si,si	! si源变址寄存器，一般与段寄存器DS联用
	sub	di,di	! di目地变址寄存器，一般与段寄存器ES联用
	rep			! 前缀指令，有调用约定
	movw		! 上述代码完成把ds段(0x1000:0)内0x2000*2字节数据复制到es段指定的内(0x0:0)，物理内存0处开始
	mov	ax, #BOOTSEG
	mov	ds, ax
	lidt	idt_48		! load idt with 0,0， 装载中断向量表，此次为空
	lgdt	gdt_48		! load gdt with whatever appropriate， 装载全局描述符表

! absolute address 0x00000, in 32-bit protected mode.
	mov	ax,#0x0001	! protected mode (PE) bit
	lmsw	ax		! This is it! 处理器切换到保护模式
	jmpi	0,8		! jmp offset 0 of segment 8 (cs)， 
					! 处理器已经工作在保护模式，所以 jmpi 中的“段基址” 8 不再表示实模式下的段基址，而是保护模式下的段选择子。
					! 按照段选择子数据格式，8 （二进制形式为 0000 0000 0000 1000）表示该段选择子请求特权级为 0 （RPL=00）、
					! 所指向的描述符存放在 GDT （TI=0）、所指向的描述符索引为 1（DI=0000 0000 0000 1）。所以，
					! 这一句汇编表示的是“选择 GDT 中的 1 号描述符，并根据该描述符内容和偏移（jmpi 0,8）跳转至内存某一位置”
					! 即跳转到内核代码段地址0处开始执行内核代码
					! 从下面定义我们可以看出从地址0开始为中断向量表，但是它没有定义表项
					! 定义了0x7ff即256个全局描述符表，起始地址是0x7c00 + gdt地址(0x7xxx)，这里定义了三个条目，第一条为空
					! 内核代码也运行在保护模式下，你曾经知道嘛？
					
					
gdt:	.word	0,0,0,0		! dummy

	.word	0x07FF		! 8Mb - limit=2047 (2048*4096=8Mb) !内核代码段描述符，8mb
	.word	0x0000		! base address=0x00000
	.word	0x9A00		! code read/exec
	.word	0x00C0		! granularity=4096, 386

	.word	0x07FF		! 8Mb - limit=2047 (2048*4096=8Mb) !内核数据段描述符，同样8mb
	.word	0x0000		! base address=0x00000
	.word	0x9200		! data read/write
	.word	0x00C0		! granularity=4096, 386

idt_48: .word	0		! idt limit=0 ! 中断向量表大小
	.word	0,0		! idt base=0L ! 起始地址为0
gdt_48: .word	0x7ff		! gdt limit=2048, 256 GDT entries ! 全局描述符表大小，每个表项8个字节
	.word	0x7c00+gdt,0	! gdt base = 07xxx
.org 510					! 地位伪指令, 指明后面代码开始地址是510, 以下代码完成将地址510、511设置为0x55、0xAA
	.word   0xAA55			!（旧版）BIOS检查可引导设备的引导签名，即所谓的幻数。引导签名位于引导扇区（扇区号0）中，它分别在字节偏移量510和511处包含字节序列0x55、0xAA。


! 说明
! 将源操作数中的值加载到全局描述符表格寄存器 (GDTR) 或中断描述符表格寄存器 (IDTR)。源操作数指定 6 字节内存位置，它包含全局描述符表格 (GDT) 或中断描述符表格 (IDT) 
! 的基址（线性地址）与限制（表格大小，以字节计）。如果操作数大小属性是 32 位，则将 16 位限制（6 字节数据操作数的 2 个低位字节）与 32 位基址（数据操作数的 4 个高位字节）
! 加载到寄存器。如果操作数大小属性是 16 位，则加载 16 位限制（2 个低位字节）与 24 位基址（第三、四、五字节）。这里，不使用操作数的高位字节，GDTR 或 IDTR 中基址的高位字节用零填充。

! LGDT 与 LIDT 指令仅用在操作系统软件中；它们不用在应用程序中。在保护模式中，它们是仅有的能够直接加载线性地址（即，不是段相对地址）与限制的指令。它们通常在实地址模式中执行，
! 以便处理器在切换到保护模式之前进行初始化。