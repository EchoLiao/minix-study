!	masterboot 2.0 - Master boot block code		Author: Kees J. Bot
!
! This code may be placed in the first sector (the boot sector) of a floppy,
! hard disk or hard disk primary partition.  There it will perform the
! following actions at boot time:
!
! - If the booted device is a hard disk and one of the partitions is active
!   then the active partition is booted.
!
! - Otherwise the next floppy or hard disk device is booted, trying them one
!   by one.
!
! To make things a little clearer, the boot path might be:
!	/dev/fd0	- Floppy disk containing data, tries fd1 then d0
!	[/dev/fd1]	- Drive empty
!	/dev/c0d0	- Master boot block, selects active partition 2
!	/dev/c0d0p2	- Submaster, selects active subpartition 0
!	/dev/c0d0p2s0	- Minix bootblock, reads Boot Monitor /boot
!	Minix		- Started by /boot from a kernel image in /minix

! boot sequence:
! 1. If a hard drive partition:
! masterboot(this code) -> bootstrap -> bootmonitor -> boot_MinixOperatingSystem
! 2. If a hard drive subpartition is booted:
! masterboot(this code) -> masterboot(again) -> bootstrap -> bootmonitor -> boot_MinixOperatingSystem


	LOADOFF	   =	0x7C00	! 本程序自动被Bios加载到这里.
	BUFFER	   =	0x0600	! First free memory
	PART_TABLE =	   446	! 分区表的入口位置,也即是代码段的大小. 1BE
	PENTRYSIZE =	    16	! 分区表每项16字节.(共4项, 共64字节)
	MAGIC	   =	   510	! Location of the AA55 magic number ! (1FE, 1FF)

	! <ibm/partition>.h:
	bootind	   =	     0
	sysind	   =	     4
	lowsec	   =	     8

.text

! Find active (sub)partition, load its first sector, run it.
master:
	xor	ax, ax
	mov	ds, ax
	mov	es, ax
	cli					! 给ss,sp传值前要关闭中断响应
	mov	ss, ax			! ds = es = ss = Vector segment
	mov	sp, #LOADOFF
	sti					! 重新打开中断响应

	! 复制本程序到BUFFER处去执行
	mov	si, sp			! si = start of this code
	push	si			! Also where we'll return to eventually
	mov	di, #BUFFER		! Buffer area
	mov	cx, #512/2		! One sector
	cld			! 方向位清零, 使得复制到的是0:LOADOFF+512而不是0:LOADOFF-512 
				! rep: repeats execution of string instructions while cx != 0.
    rep	movs	! 从DS:SI复制数据到ES:DI.(一次复制1 word而不是1 bite) ! ?????
	jmpf	BUFFER+migrate, 0	! far jmp. 0:BUFFER+migrate
!
!上面的代码块把mbr的512字节移动到0x0000:0x0600处,然后跳转到migrate的代码处执行.
!接下来首先测试设备是否可引导，即dl的最高位是否为1, 为1则设备可引导; 否
!则跳转到nextdisk处.
!如果可引导则检测的分区，测试其类型同时是否是活动分区，总共测试4次，如果
!还没有找到则显示没有活动分区,然后直接跳到reboot的地方执行,显示一些信息后系统重启.
!
migrate:
! Find the active partition
findactive:
	! ## testb sets the sign flag if the value in dl is negative. Remember that 0x00
	! and 0x01 correspond to the first and second floppy drives and 0x80, 0x81,
	! 0x82, and 0x83 correspond to hard drives 1-4.
	testb	dl, dl		! dl是负的吗?若是,则符号标记位被置位. dl的值从何来?????
	jns	nextdisk		! 如果为正数的话,是软盘(软盘上没有bootable),则跳转到nextdisk
	mov	si, #BUFFER+PART_TABLE
find:	cmpb	sysind(si), #0	! Partition type, nonzero when in use
	jz	nextpart
	testb	bootind(si), #0x80	! Active partition flag in bit 7
	jz	nextpart		! It's not active
loadpart:
	call	load			! Load partition bootstrap
	jc	error1			! Not supposed to fail
bootstrap:
	ret				! Jump to the master bootstrap
nextpart:
	add	si, #PENTRYSIZE
	cmp	si, #BUFFER+PART_TABLE+4*PENTRYSIZE ! 最多有4个主分区
	jb	find
! No active partition, tell 'em
	!利用call命令会把下一条要执行的指令的地址压人堆栈的特点吧.ascii字符的初始地址给压人了堆栈
	!这样当进入print函数后就可以通过弹栈把字符的首地址取出，最后字符显示完后直接用jmp
	!(si)指令跳转到下一个地方继续执行
	call	print
	.ascii	"No active partition\0"
	jmp	reboot

! There are no active partitions on this drive, try the next drive.
nextdisk:
	incb	dl			! Increment dl for the next drive
	testb	dl, dl
	js	nexthd			! Hard disk if negative
	int	0x11			! Get equipment configuration
	shl	ax, #1			! Highest floppy drive # in bits 6-7
	shl	ax, #1			! Now in bits 0-1 of ah
	andb	ah, #0x03		! Extract bits
	cmpb	dl, ah			! Must be dl <= ah for drive to exist
	ja	nextdisk		! Otherwise try disk 0 eventually
	call	load0			! Read the next floppy bootstrap
	jc	nextdisk		! It failed, next disk please
	ret				! Jump to the next master bootstrap
nexthd:	call	load0			! Read the hard disk bootstrap
error1:	jc	error			! No disk?
	ret


! Load sector 0 from the current device.  It's either a floppy bootstrap or
! a hard disk master bootstrap.
load0:
	mov	si, #BUFFER+zero-lowsec	! si = where lowsec(si) is zero
	!jmp	load

! Load sector lowsec(si) from the current device.  The obvious head, sector,
! and cylinder numbers are ignored in favour of the more trustworthy absolute
! start of partition.
load:
	mov	di, #3		! Three retries for floppy spinup
retry:	push	dx		! Save drive code
	push	es
	push	di		! Next call destroys es and di
	movb	ah, #0x08	! Code for drive parameters
	int	0x13
	pop	di
	pop	es
	andb	cl, #0x3F	! cl = max sector number (1-origin) ! cl&63
	incb	dh		! dh = 1 + max head number (0-origin)
	movb	al, cl		! al = cl = sectors per track
	mulb	dh		! dh = heads, ax = heads * sectors
	mov	bx, ax		! bx = sectors per cylinder = heads * sectors
	mov	ax, lowsec+0(si)
	mov	dx, lowsec+2(si)! dx:ax = sector within drive
	cmp	dx, #[1024*255*63-255]>>16  ! Near 8G limit? ! 只须比较lowsec高16位. 8GB约等于1024*255*63扇区
	jae	bigdisk
		! div:	if the source
        ! divisor is a byte value then ax is divided by "src" and the quotient
        ! is placed in al and the remainder in ah.  if source operand is a word
        ! value, then dx:ax is divided by "src" and the quotient is stored in ax
        ! and the remainder in dx.
	div	bx		! ax = cylinder, dx = sector within cylinder
	xchg	ax, dx		! ax = sector within cylinder, dx = cylinder
	movb	ch, dl		! ch = low 8 bits of cylinder
	divb	cl		! al = head, ah = sector (0-origin)
	xorb	dl, dl		! About to shift bits 8-9 of cylinder into dl
	shr	dx, #1
	shr	dx, #1		! dl[6..7] = high cylinder
	orb	dl, ah		! dl[0..5] = sector (0-origin)
	movb	cl, dl		! cl[0..5] = sector, cl[6..7] = high cyl
	incb	cl		! cl[0..5] = sector (1-origin)
	pop	dx		! Restore drive code in dl
	movb	dh, al		! dh = al = head
	mov	bx, #LOADOFF	! es:bx = where sector is loaded
	mov	ax, #0x0201	! Code for read, just one sector	ah=2 al=1
	! INT 13 - DISK - READ SECTOR(S) INTO MEMORY
		! AH = 02h
		! AL = number of sectors to read (must be nonzero)
		! CH = low eight bits of cylinder number
		! CL = sector number 1-63 (bits 0-5) high two bits of cylinder (bits 6-7, hard disk only)
		! DH = head number
		! DL = drive number (bit 7 set for hard disk)
		! ES:BX -> data buffer
	! Return: CF set on error
		! if AH = 11h (corrected ECC error), AL = burst length
		! CF clear if successful
		! AH = status (see #00234)
		! AL = number of sectors transferred (only valid if CF set for some BIOSes)
	int	0x13		! Call the BIOS for a read
	jmp	rdeval		! Evaluate read result
bigdisk:
	mov	bx, dx		! bx:ax = dx:ax = sector to read
	pop	dx		! Restore drive code in dl
	push	si		! Save si
	mov	si, #BUFFER+ext_rw ! si = extended read/write parameter packet
	mov	8(si), ax	! Starting block number = bx:ax
	mov	10(si), bx
	movb	ah, #0x42	! Extended read
	int	0x13
	pop	si		! Restore si to point to partition entry
	!jmp	rdeval
rdeval:
	jnc	rdok		! Read succeeded
	cmpb	ah, #0x80	! Disk timed out?  (Floppy drive empty)
	je	rdbad
	dec	di
	jl	rdbad		! Retry count expired
	xorb	ah, ah
	! INT 13 - DISK - RESET DISK SYSTEM
		! AH = 00h
		! DL = drive (if bit 7 is set both hard disks and floppy disks reset)
	! Return: AH = status (see #00234)
		! CF clear if successful (returned AH=00h)
		! CF set on error
	int	0x13		! Reset ! CF set on error
	jnc	retry		! Try again
rdbad:	stc			! Set carry flag
	ret
rdok:	cmp	LOADOFF+MAGIC, #0xAA55
	jne	nosig		! Error if signature wrong
	ret			! Return with carry still clear
nosig:	call	print
	.ascii	"Not bootable\0"
	jmp	reboot

! A read error occurred, complain and hang
error:
	mov	si, #LOADOFF+errno+1
prnum:	movb	al, ah		! Error number in ah
	andb	al, #0x0F	! Low 4 bits
	cmpb	al, #10		! A-F?
	jb	digit		! 0-9!
	addb	al, #7		! 'A' - ':'
digit:	addb	(si), al	! Modify '0' in string
	dec	si
	movb	cl, #4		! Next 4 bits
	shrb	ah, cl
	jnz	prnum		! Again if digit > 0
	call	print
	.ascii	"Read error "
errno:	.ascii	"00\0"
	!jmp	reboot

reboot:
	call	print
	.ascii	".  Hit any key to reboot.\0"
	xorb	ah, ah		! Wait for keypress ! ah=0
	int	0x16 ! 读键盘字符
	call	print
	.ascii	"\r\n\0"
	int	0x19 ! 重新引导装入程序, 即重起

! Print a message.
print:	pop	si		! si = address of String following 'call print'
prnext:	lodsb			! al = *si++ is char to be printed
	testb	al, al		! Null marks end
	jz	prdone
	movb	ah, #0x0E	! Print character in teletype mode
	mov	bx, #0x0001	! Page 0, foreground color
	int	0x10 ! ah=0E, int 0x10 显示字符并自动处理光标; al=要显示的字符, bh=页号
	jmp	prnext
prdone:	jmp	(si)		! Continue after the string

.data

! Extended read/write commands require a parameter packet.
ext_rw:
	.data1	0x10		! Length of extended r/w packet
	.data1	0		! Reserved
	.data2	1		! Blocks to transfer (just one)
	.data2	LOADOFF		! Buffer address offset
	.data2	0		! Buffer address segment
	.data4	0		! Starting block number low 32 bits (tbfi)
# zero:	.data4	0		! Starting block number high 32 bits


!硬盘第一扇区结构:
	! 0000   |------------------------------------------------|
		   ! |                                                |
		   ! |                                                |
		   ! |                Master Boot Record	          |
		   ! |                                                |
		   ! |                                                |
		   ! |                主引导记录(446字节)		      |
		   ! |                                                |
		   ! |                                                |
		   ! |                                                |
	! 01BD   |                                                |
    ! 01BE 446------------------------------------------------|
		   ! |                                                |
	! 01CD   |              分区信息   1(16字节)              |
	! 01CE   |------------------------------------------------|
		   ! |                                                |
	! 01DD   |              分区信息   2(16字节)              |
	! 01DE   |------------------------------------------------|
		   ! |                                                |
	! 01ED   |              分区信息   3(16字节)              |
	! 01EE   |------------------------------------------------|
		   ! |                                                |
	! 01FD   |              分区信息   4(16字节)              |
		   ! |------------------------------------------------|      
		   ! | 01FE                    | 01FF                 |
		   ! |          55             |            AA        |
		   ! |------------------------------------------------| 