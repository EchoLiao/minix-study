! 这段代码即为bootstrap（存放在bootblock.s文件中），会将boot monitor拷贝到地址
! BOOTSEG处，并跳转到BOOTSEG:BOOTOFF处执行。
!	Bootblock 1.5 - Minix boot block.		Author: Kees J. Bot
!					   			21 Dec 1991
!
! When the PC is powered on, it will try to read the first sector of floppy
! disk 0 at address 0x7C00.  If this fails due to the absence of flexible
! magnetic media, it will read the master boot record from the first sector
! of the hard disk.  This sector not only contains executable code, but also
! the partition table of the hard disk.  When executed, it will select the
! active partition and load the first sector of that at address 0x7C00.
! This file contains the code that is eventually read from either the floppy
! disk, or the hard disk partition.  It is just smart enough to load /boot
! from the boot device into memory at address 0x10000 and execute that.  The
! disk addresses for /boot are patched into this code by installboot as 24-bit
! sector numbers and 8-bit sector counts above enddata upwards.  /boot is in
! turn smart enough to load the different parts of the Minix kernel into
! memory and execute them to finally get Minix started.
!

	LOADOFF	   =	0x7C00	! 0x0000:LOADOFF is where this code is loaded
	BOOTSEG    =	0x1000	! Secondary boot code segment. ! 64KB(以后)
	BOOTOFF	   =	0x0030	! Offset into /boot above header
	BUFFER	   =	0x0600	! First free memory
	LOWSEC     =	     8	! Offset of logical first sector in partition
				! table

	! Variables addressed using bp register
	device	   =	     0	! The boot device
	lowsec	   =	     2	! Offset of boot partition within drive
	secpcyl	   =	     6	! Sectors per cylinder = heads * sectors

!### 功能:
!		把boot加载到内存, 并跳转到那里去执行.
!### 入口参数:
!		es:si = partition table entry if hard disk
!###
!		读取完boot后, 直接跳转到那(BOOTSEG:BOOTOFF)去运行.

.text

! Start boot procedure.

boot:
	xor	ax, ax		! ax = 0x0000, the vector segment
	mov	ds, ax
	cli			! Ignore interrupts while setting stack
	mov	ss, ax		! ss = ds = vector segment
	mov	sp, #LOADOFF	! Usual place for a bootstrap stack
	sti

	push	ax
	push	ax		! Push a zero lowsec(bp)

	push	dx		! Boot device in dl will be device(bp)
	mov	bp, sp		! Using var(bp) is one byte cheaper then var.

	push	es
	push	si		! es:si = partition table entry if hard disk

	mov	di, #LOADOFF+sectors	! char *di = sectors;

	testb	dl, dl		! Winchester disks if dl >= 0x80
	jge	floppy

winchester:

! Get the offset of the first sector of the boot partition from the partition
! table.  The table is found at es:si, the lowsec parameter at offset LOWSEC.

    ! Using cseg/dseg/eseg/fseg/gseg/sseg to indicate addressing in
    ! cs/ds/es/fs/gs/ss segment register
	eseg
	les	ax, LOWSEC(si)	  ! es:ax = LOWSEC+2(si):LOWSEC(si). !!!!!
	mov	lowsec+0(bp), ax  ! Low 16 bits of partition's first sector
	mov	lowsec+2(bp), es  ! High 16 bits of partition's first sector

! Get the drive parameters, the number of sectors is bluntly written into the
! floppy disk sectors/track array.

	movb	ah, #0x08	! Code for drive parameters
    ! ! INT 13 - DISK - GET DRIVE PARAMETERS (PC,XT286,CONV,PS,ESDI,SCSI)
        ! ! AH = 08h
        ! ! DL = drive (bit 7 set for hard disk)
        ! ! ES:DI = 0000h:0000h to guard against BIOS bugs
    ! ! Return: CF set on error
        ! ! CF clear if successful
            ! ! AH = 00h
            ! ! BL = drive type (AT/PS2 floppies only) (see #00242)
            ! ! CH = low eight bits of maximum cylinder number
            ! ! CL = maximum sector number (bits 5-0) high two bits of maximum cylinder number (bits 7-6)
            ! ! DH = maximum head number
            ! ! DL = number of drives
            ! ! ES:DI -> drive parameter table (floppies only)
	int	0x13		! dl still contains drive
	andb	cl, #0x3F	! cl = max sector number (1-origin)
	movb	(di), cl	! Number of sectors per track ! ?????
	incb	dh		! dh = 1 + max head number (0-origin)
	jmp	loadboot

! Floppy:
! Execute three read tests to determine the drive type.  Test for each floppy
! type by reading the last sector on the first track.  If it fails, try a type
! that has less sectors.  Therefore we start with 1.44M (18 sectors) then 1.2M
! (15 sectors) ending with 720K/360K (both 9 sectors).

next:	inc	di		! Next number of sectors per track

floppy:	xorb	ah, ah		! Reset drive
	int	0x13

	movb	cl, (di)	! cl = number of last sector on track

	cmpb	cl, #9		! No need to do the last 720K/360K test
	je	success

! Try to read the last sector on track 0

	mov	es, lowsec(bp)	! es = vector segment (lowsec = 0)
	mov	bx, #BUFFER	! es:bx buffer = 0x0000:0x0600
	mov	ax, #0x0201	! Read sector, #sectors = 1
	xorb	ch, ch		! Track 0, last sector
	xorb	dh, dh		! Drive dl, head 0
	int	0x13
	jc	next		! Error, try the next floppy type

success:movb	dh, #2		! Load number of heads for multiply

loadboot:
! Load /boot from the boot device

	movb	al, (di)	! al = (di) = sectors per track
	mulb	dh		! dh = heads, ax = heads * sectors.  ! ax=al*dh
	mov	secpcyl(bp), ax	! Sectors per cylinder = heads * sectors

	mov	ax, #BOOTSEG	! Segment to load /boot into
	mov	es, ax
	xor	bx, bx		! Load first sector at es:bx = BOOTSEG:0x0000
	mov	si, #LOADOFF+addresses	! Start of the boot code addresses
load:
	mov	ax, 1(si)	! Get next sector number: low 16 bits
	movb	dl, 3(si)	! Bits 16-23 for your up to 8GB partition. dl:ax=相对(于分区)逻辑扇区号
	xorb	dh, dh		! dx:ax = sector within partition
	add	ax, lowsec+0(bp)
	adc	dx, lowsec+2(bp)! dx:ax = sector within drive. dx:ax=逻辑扇区号
	cmp	dx, #[1024*255*63-255]>>16  ! Near 8G limit?
	jae	bigdisk
	div		secpcyl(bp)	! ax = cylinder, dx = sector within cylinder. ! dx:ax/src=ax...dx
	xchg	ax, dx		! ax = sector within cylinder, dx = cylinder
	movb	ch, dl		! ch = low 8 bits of cylinder
	divb	(di)		! al = head, ah = sector (0-origin)
	xorb	dl, dl		! About to shift bits 8-9 of cylinder into dl
	shr	dx, #1
	shr	dx, #1		! dl[6..7] = high cylinder
	orb	dl, ah		! dl[0..5] = sector (0-origin)
	movb	cl, dl		! cl[0..5] = sector, cl[6..7] = high cyl
	incb	cl		! cl[0..5] = sector (1-origin)
	movb	dh, al		! dh = al = head
	movb	dl, device(bp)	! dl = device to read
	movb	al, (di)	! Sectors per track - Sector number (0-origin)
	subb	al, ah		! = Sectors left on this track
	cmpb	al, (si)	! Compare with # sectors to read
	jbe	read		! Can't read past the end of a cylinder?
	movb	al, (si)	! (si) < sectors left on this track
read:	push	ax		! Save al = sectors to read
	movb	ah, #0x02	! Code for disk read (all registers in use now!)
! ! INT 13 - DISK - READ SECTOR(S) INTO MEMORY
	! ! AH = 02h
	! ! AL = number of sectors to read (must be nonzero)
	! ! CH = low eight bits of cylinder number
	! ! CL = sector number 1-63 (bits 0-5) high two bits of cylinder (bits 6-7, hard disk only)
	! ! DH = head number
	! ! DL = drive number (bit 7 set for hard disk)
	! ! ES:BX -> data buffer
! ! Return: CF set on error ! ! if AH = 11h (corrected ECC error), AL = burst length
	! ! CF clear if successful
	! ! AH = status (see #00234). ah=0
	! ! AL = number of sectors transferred (only valid if CF set for some BIOSes)
	int	0x13		! Call the BIOS for a read
	pop	cx		! Restore al in cl
	jmp	rdeval
bigdisk:
	movb	cl, (si)	! Number of sectors to read.  读多少个扇区?
	push	si		! Save si
	mov		si, #LOADOFF+ext_rw ! si = extended read/write parameter packet
	movb	2(si), cl	! Fill in # blocks to transfer
	mov		4(si), bx	! Buffer address ! 这是offset
	mov		8(si), ax	! Starting block number = dx:ax. 要读取的逻辑扇区号
	mov		10(si), dx
	movb	dl, device(bp)	! dl = device to read
	movb	ah, #0x42	! Extended read
    ! ! INT 13 - IBM/MS INT 13 Extensions - EXTENDED READ
        ! ! AH = 42h
        ! ! DL = drive number
        ! ! DS:SI -> disk address packet (see #00272)
    ! ! Return: CF clear if successful; AH = 00h
	! ! CF set on error; AH = error code (see #00234)
	int		0x13
	pop		si		! Restore si to point to the addresses array
	!jmp	rdeval
rdeval:
	jc		error		! Jump on disk read error
	movb	al, cl		! Restore al = sectors read
	addb	bh, al		! bx += 2 * al * 256 (add bytes read)
	addb	bh, al		! es:bx = where next sector must be read
	add		1(si), ax	! Update address by sectors read
	adcb	3(si), ah	! Don't forget bits 16-23 (add ah = 0). ah=0, 这里是为了加上进位
	subb	(si), al	! Decrement sector count by sectors read
	jnz		load		! Not all sectors have been read
	add		si, #4		! Next (address, count) pair. 转到下一个入口进行读取!
	cmpb	ah, (si)	! Done when no sectors to read
	jnz		load		! Read next chunk of /boot

done:

! Call /boot, assuming a long a.out header (48 bytes).  The a.out header is
! usually short (32 bytes), but to be sure /boot has two entry points:
! One at offset 0 for the long, and one at offset 16 for the short header.
! Parameters passed in registers are:
!
!	dl	= Boot-device.
!	es:si	= Partition table entry if hard disk.
!
	pop	si		! Restore es:si = partition table entry
	pop	es		! dl is still loaded
	! ! pop dx 不用此指令也可以, 因为现在dl的值仍表示Boot-device.
	jmpf	BOOTOFF, BOOTSEG  ! jmp to sec. boot (skipping header). ! BOOTOFF: 跳过文件头

! Read error: print message, hang forever
error:
	mov		si, #LOADOFF+errno+1
prnum:	
	movb	al, ah		! Error number in ah
	andb	al, #0x0F	! Low 4 bits
	cmpb	al, #10		! A-F?
	jb		digit		! 0-9!
	addb	al, #7		! 'A' - ':'
digit:	
	addb	(si), al	! Modify '0' in string
	dec		si
	movb	cl, #4		! Next 4 bits
	shrb	ah, cl
	jnz		prnum		! Again if digit > 0

	mov		si, #LOADOFF+rderr  ! String to print
print:		
	! lodsb: transfers string element addressed by ds:si (even if an operand is
	! supplied) to the accumulator.
	lodsb				! al = *si++ is char to be printed
	testb	al, al		! Null byte marks end
hang:	
	jz		hang		! Hang forever waiting for CTRL-ALT-DEL
	movb	ah, #0x0E	! Print character in teletype mode
	mov		bx, #0x0001	! Page 0, foreground color
	! ! INT 10 - VIDEO - TELETYPE OUTPUT
		! ! AH = 0Eh
		! ! AL = character to write
		! ! BH = page number
		! ! BL = foreground color (graphics modes only)
	! ! Return: nothing
	int		0x10		! Call BIOS VIDEO_IO
	jmp		print

.data
rderr:	.ascii	"Read error "
errno:	.ascii	"00 \0"
errend:

! Floppy disk sectors per track for the 1.44M, 1.2M and 360K/720K types:
sectors:
	.data1	18, 15, 9

! Extended read/write commands require a parameter packet.
ext_rw:
	.data1	0x10		! Length of extended r/w packet ! 这些被转入的参数所占的空间大小
	.data1	0		! Reserved
	.data2	0		! Blocks to transfer (to be filled in)
	.data2	0		! Buffer address offset (tbfi)
	.data2	BOOTSEG		! Buffer address segment
	.data4	0		! Starting block number low 32 bits (tbfi)
	.data4	0		! Starting block number high 32 bits

	.align	2
addresses:
! The space below this is for disk addresses for a 38K /boot program (worst
! case, i.e. file is completely fragmented).  It should be enough.

                ! ! |    .    |
                ! ! |    .    |                                |           |
                ! ! |    .    |                              . |-----------|
                ! ! |---------|                              . |           |
                ! ! |         |                            # . |-----------|
        !#! BOOTSEG |---------| 0x10000                        |      dl   |
                ! ! |    .    |                              3 |-----------| \
                ! ! |    .    |                                |           |  |
                ! ! |    .    |                              2 |------ax---|  |=逻辑扇区号(相对于分区首)
                ! ! |---------|                                |           |  |
   !每道扇区数 <====|  63     |                              1 |-----------| /
                ! ! |---------| di #LOADOFF+sectors            |      num  |===>下一个要读的扇区个为数
                ! ! |    .    |                            # 0 |-----------|  si
                ! ! |    .    |                                |   dl      |\
                ! ! |    .    |                              3 |-----------| |
                ! ! |---------|                                |           | |=逻辑扇区号(相对于分区首)
      ! 255*63 <====| 16065   |                              2 |---ax------| |
        !#! LOADOFF |---------| 0x7C00  sp(开始时)             |           |/
              ! ! /=|  ax     |                              1 |-----------|
   ! !保存起始逻 /  |---------|                                |   num     |===>要读的扇区个为数
   ! !辑扇区号 <====|  ax     |                            # 0 |-----------|  si #LOADOFF+addresses
                ! ! |---------|                                |           | (Start of the boot code addresses)
                ! ! |  dx     |                                |           |
                ! ! |---------| bp                      ! Figure 2: 要读取的扇区号和个数预先被保存在这里
                ! ! |  es     |
                ! ! |---------|
                ! ! |  si     |
                ! ! |---------|
                ! ! |         |
                ! ! |---------|
                ! ! |    .    |
                ! ! |    .    |
                ! ! |    .    |
                ! ! |    .    |
         !#! BUFFER |---------|
                ! ! |         |
                ! ! |---------|
                ! ! |         |
	! Figure 1: 开始时的内存和栈结构


    ! ================================================================================
    ! |    字节                                           含义                       |
  ! 0 ================================================================================
    ! |  0 (bootind)          Activeflag.活动标志.若为0x80H,则表示该分区为活动分区.若|
    ! |                      为0x00H,则表示该分区为非活动分区.                       |
    ! --------------------------------------------------------------------------------
    ! |                       该分区的起始可用的空间的磁头号,扇区号,柱面号. 磁头号:第|
    ! |1,2,3 (last_head,      1字节, 扇区号:第2字节低6位, 柱面号:第2字节高2位 + 第3字|
    ! | last_sec,last_cyl)    节.                                                    |
  ! 4 --------------------------------------------------------------------------------
    ! |                       分区文件系统标志:                                      |
    ! |                                   分区未用: 0x00H.                           |
    ! |                                   扩展分区: 0x05H, 0x0FH.                    |
    ! |  4 (sysind)                       FAT16分区: 0x06H.                          |
    ! |                                   FAT32分区: 0x0BH, 0x1BH, 0x0CH, 0x1CH.     |
    ! |                                   NTFS分区: 0x07H.                           |
    ! --------------------------------------------------------------------------------
    ! |5,6,7 (last_head,      该分区的结束磁头号,扇区号,柱面号, 含义同上.            |
    ! | last_sec,last_cyl)                                                           |
  ! 8 --------------------------------------------------------------------------------
    ! |8,9,10,11 (lowsec)     逻辑起始扇区号. 即该分区起点之前已用了的扇区数.        |
 ! 12 --------------------------------------------------------------------------------
    ! |12,13,14,15 (size)     该分区所占用的扇区数.                                  |
    ! ================================================================================
   				! Table 1: 分区表每项16各个字节表示的意义


