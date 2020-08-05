;*******************************
; FileName: src/boot/loader.asm
; Author: WenShiHuai
; CreateDate: 2020/8/5
; Describe: 加载程序
;*******************************

	org  0100h			; 实际是偏移值

	jmp	LABEL_START		; Start

; 下面是 FAT12 磁盘的头, 之所以包含它是因为下面用到了磁盘的一些信息
%include	"fat12hdr.inc"
%include	"load.inc"
%include	"pm.inc"

;GDT属性                          段基址       段界限       段属性
LABEL_GDT:          Descriptor     0,          0,          0            		 ;空描述符
LABEL_DESC_FLAT_C:  Descriptor     0,      0fffffh, 	DA_CR|DA_32|DA_LIMIT_4K  ;0-4G
LABEL_DESC_FLAT_RW: Descriptor     0,      0fffffh, 	DA_DRW|DA_32|DA_LIMIT_4K ;0-4G
LABEL_DESC_VIDEO:   Descriptor  0B8000h,    0ffffh, 	DA_DRW|DA_DPL3           ;显存首地址
;GDT结束

GdtLen      equ    $  -  LABEL_GDT      ;GDT长度
GdtPtr      dw     GdtLen - 1           ;GDT界限  
            dd     BaseOfLoaderPhyAddr + LABEL_GDT                    ;GDT基地址

; GDT 选择子
SelectorFlatC		equ	LABEL_DESC_FLAT_C	- LABEL_GDT
SelectorFlatRW		equ	LABEL_DESC_FLAT_RW	- LABEL_GDT
SelectorVideo		equ	LABEL_DESC_VIDEO	- LABEL_GDT + SA_RPL3

BaseOfStack			equ		0100h
PageDirBase			equ		100000h	; 页目录开始地址: 1M
PageTblBase			equ		101000h	; 页表开始地址:   1M + 4K

LABEL_START:		 ; 初始化代码段和数据段
	mov	ax, cs
	mov	ds, ax
	mov	es, ax
	mov	ss, ax
	mov	sp, BaseOfStack

	mov	dh, 0				 ; "Loading  "
	call	DispStrRealMode	 ; 实模式下显示字符串，保护模式下用DispStr

	; 得到内存数
	mov	ebx, 0				 ; ebx = 后续值, 开始时需为 0
	mov	di, _MemChkBuf		 ; es:di 指向一个地址范围描述符结构(ARDS)
.MemChkLoop:
	mov	eax, 0E820h			 ; eax = 0000E820h
	mov	ecx, 20				 ; ecx = 地址范围描述符结构的大小
	mov	edx, 0534D4150h		 ; edx = 'SMAP'
	int	15h					 ; int 15h
	jc	.MemChkFail
	add	di, 20
	inc	dword [_dwMCRNumber] ; dwMCRNumber = ARDS 的个数 调用一次15H中断就加一
	cmp	ebx, 0
	jne	.MemChkLoop
	jmp	.MemChkOK
.MemChkFail:
	mov	dword [_dwMCRNumber], 0
.MemChkOK:
    xor	ah, ah				 ; 软驱复位
	xor	dl, dl	     
	int	13h

	; 在软盘的根目录寻找 KERNEL.BIN
	; 初始化扇区查找的开始区域
	mov	word [wSectorNo], SectorNoOfRootDirectory	; SectorNoOfRootDirectory根目录开始扇区号


LABEL_SEARCH_IN_ROOT_DIR_BEGIN:
	cmp	word [wRootDirSizeForLoop], 0	; 判断根目录区是不是已经读完
	jz	LABEL_NO_KERNELBIN				; 如果读完表示没有找到 KERNEL.BIN,跳转到没有找到

	dec	word [wRootDirSizeForLoop]		; 查找扇区向前移动一位
	mov	ax, BaseOfKernelFile
	mov	es, ax							; es <- BaseOfKernelFile
	mov	bx, OffsetOfKernelFile			; bx <- OffsetOfKernelFile
	mov	ax, [wSectorNo]					; ax <- Root Directory 中的某 Sector 号

	mov	cl, 1							; 连续读取的扇区数
	call	ReadSector

	mov	si, KernelFileName				; ds:si -> "KERNEL  BIN"
	mov	di, OffsetOfKernelFile
	cld									; 使字符串操作中SI或DI地址指针自动增加，字符串处理由前往后
	mov	dx, 10h							; 循环的次数 = 512 / 32个文件目录项	

LABEL_SEARCH_FOR_KERNELBIN:				; 寻找kernel.bin文件
	cmp	dx, 0				  				; 循环次数控制
	jz	LABEL_GOTO_NEXT_SECTOR_IN_ROOT_DIR	; 如果已经读完了一个 Sector,就跳到下一个 Sector
	dec	dx
	mov	cx, 11								; 查找文件的文件名长度

LABEL_CMP_FILENAME:	; 对比文件名字符串
	cmp	cx, 0
	jz	LABEL_FILENAME_FOUND	; 如果比较了 11 个字符都相等, 表示找到

	dec	cx
	lodsb						; ds:si -> al 块读出指令
	cmp	al, byte [es:di]		; 字节对比
	jz	LABEL_GO_ON				; 相等就继续对比
	jmp	LABEL_DIFFERENT			; 只要发现不一样的字符就表明本 DirectoryEntry 不是

LABEL_GO_ON:
	inc	di
	jmp	LABEL_CMP_FILENAME		; 继续循环

LABEL_DIFFERENT:
	and	di, 0FFE0h				; 1111 1111 1110 0000 di &= E0 为了让它指向本条目开头
	add	di, 20h					; 文件目录占32字节 di += 20h  下一个目录条目
	mov	si, KernelFileName
	jmp	LABEL_SEARCH_FOR_KERNELBIN

LABEL_GOTO_NEXT_SECTOR_IN_ROOT_DIR:
	add	word [wSectorNo], 1
	jmp	LABEL_SEARCH_IN_ROOT_DIR_BEGIN

LABEL_NO_KERNELBIN:
	mov	dh, 2			; "No KERNEL."
	call	DispStr		; 显示字符串
	jmp	$

LABEL_FILENAME_FOUND:		; 找到 KERNEL.BIN 继续找文件数据
	mov	ax, RootDirSectors	; 根目录大小
	and	di, 0FFF0h			; di -> 当前条目的开始

	push	eax
	mov	eax, [es : di + 01Ch]		; 获取 KERNEL.BIN 文件大小
	mov	dword [dwKernelSize], eax	; 保存 KERNEL.BIN 文件大小
	pop	eax

	add	di, 01Ah					; 根目录偏移，此条目对应的开始簇号
	mov	cx, word [es:di]
	push	cx						; 保存此 Sector 在 FAT 中的簇号(只有簇号信息 >= 0x2h)
	add	cx, ax
	add	cx, DeltaSectorNo			; cl <- KERNEL.BIN 的起始扇区号(逻辑)
	mov	ax, BaseOfKernelFile
	mov	es, ax						; es <- BaseOfKernelFile
	mov	bx, OffsetOfKernelFile		; bx <- OffsetOfKernelFile
	mov	ax, cx						; ax <- Sector 号（逻辑扇区号）

LABEL_GOON_LOADING_FILE:
	push	ax
	push	bx
	mov	ah, 0Eh				;  每读一个扇区就在 "Loading  " 后面
	mov	al, '.'				;  打一个点, 形成这样的效果:
	mov	bl, 0Fh	
	int	10h
	pop	bx
	pop	ax

	mov	cl, 1
	call	ReadSector
	pop	ax					; 取出此 Sector 在 FAT 中的序号(簇号)
	call	GetFATEntry
	cmp	ax, 0FFFh
	jz	LABEL_FILE_LOADED
	push	ax				; 保存 Sector 在 FAT 中的序号
	mov	dx, RootDirSectors
	add	ax, dx
	add	ax, DeltaSectorNo
	add	bx, [BPB_BytsPerSec]
	jmp	LABEL_GOON_LOADING_FILE
	
LABEL_FILE_LOADED:
	call	KillMotor				; 关闭软驱马达

	mov	dh, 1						; "Ready."
	call	DispStrRealMode		    ; 显示字符串

	; 下面准备跳入保护模式

	; 加载 GDTR
	lgdt	[GdtPtr]

	; 关中断
	cli

	; 打开地址线A20
	in	al, 92h
	or	al, 00000010b
	out	92h, al

	; 准备切换到保护模式
	mov	eax, cr0
	or	eax, 1
	mov	cr0, eax

	; 真正进入保护模式
	jmp	dword SelectorFlatC:(BaseOfLoaderPhyAddr+LABEL_PM_START)

	jmp	$


;----------------------------------------------------------------------------
;变量
;----------------------------------------------------------------------------
wRootDirSizeForLoop	dw	RootDirSectors	; Root Directory 占用的扇区数
wSectorNo			dw	0				; 要读取的扇区号
bOdd				db	0				; 奇数还是偶数
dwKernelSize		dd	0				; KERNEL.BIN 文件大小

;----------------------------------------------------------------------------
;字符串
;----------------------------------------------------------------------------
KernelFileName		db	"KERNEL  BIN", 0	; KERNEL.BIN 之文件名
; 为简化代码, 下面每个字符串的长度均为 MessageLength
MessageLength		equ	9
LoadMessage:		db	"Loading  "
Message1			db	"Ready.   "
Message2			db	"No KERNEL"
;----------------------------------------------------------------------------

;----------------------------------------------------------------------------
; 函数名: DispStrRealMode
;----------------------------------------------------------------------------
; 作用:显示一个字符串, 函数开始时 dh 中应该是字符串序号(0-based)
; ---------------------------------------------------------------------------
DispStrRealMode:
	mov	ax, MessageLength
	mul	dh
	add	ax, LoadMessage		; 字符串偏移地址
	mov	bp, ax				; 字符串地址放入ES：BP
	mov	ax, ds
	mov	es, ax
	mov	cx, MessageLength	; CX = 串长度
	mov	ax, 01301h			; AH = 13,  AL = 01h
	mov	bx, 0007h			; 页号为0(BH = 0) 黑底白字(BL = 07h)
	mov	dl, 0
	add	dh, 3				; 从第 3 行往下显示
	int	10h					; int 10h
	ret

;----------------------------------------------------------------------------
; 函数名: ReadSector
;----------------------------------------------------------------------------
; 作用:从序号(Directory Entry 中的 Sector 号)为 ax 的的 Sector 开始, 将 cl 个 Sector 读入 es:bx 中
;----------------------------------------------------------------------------
; 通过扇区号求磁道和柱面号,设逻辑扇区号为 x
; 编号顺序
; 磁头号		柱面号		扇区号			逻辑扇区号
; 	0			 0			1				 0
; 	0			 0		    ...				 ...
; 	1			 0			1				 18
; 	1			 0			...				 ...
; x / 每磁道扇区数 = y(商) ... z(余数)
; 磁头号 = y & 1 ... 磁头要么为0,要么为1 	0 1 0 1 0 1 0 1
; 柱面号 = y / 2 (y >> 1) 成对出现的       0 0 1 1 2 2 3 3
; 起始扇区号 = z + 1
; ---------------------------------------------------------------------------
ReadSector:
	push	bp				; bp用于堆栈寻址
	mov	bp, sp
	sub	esp, 2				; 开辟出两个字节的堆栈区域保存要读的扇区数: byte [bp-2]

	mov	byte [bp-2], cl 	; cl 个 Sector
	push	bx				; 保存 bx
	mov	bl, [BPB_SecPerTrk]	; bl: 除数
	div	bl					; y 在 al 中, z 在 ah 中
	inc	ah					; z ++
	mov	cl, ah				; cl <- 起始扇区号
	mov	dh, al				; dh <- y
	shr	al, 1				; y >> 1 (其实是 y/BPB_NumHeads, 这里BPB_NumHeads=2)
	mov	ch, al				; ch <- 柱面号
	and	dh, 1				; dh & 1 = 磁头号
	pop	bx					; 恢复 bx
	; 至此, "柱面号, 起始扇区, 磁头号" 全部得到
	mov	dl, [BS_DrvNum]		; 驱动器号 (0 表示 A 盘)
.GoOnReading:
	mov	ah, 2				; 读
	mov	al, byte [bp-2]		; 读 al 个扇区
	int	13h
	jc	.GoOnReading		; 如果读取错误 CF 会被置为 1, 这时就不停地读, 直到正确为止

	add	esp, 2
	pop	bp

	ret
; ---------------------------------------------------------------------------

;----------------------------------------------------------------------------
; 函数名: GetFATEntry
;----------------------------------------------------------------------------
; 作用:
;	找到序号为 ax 的 Sector 在 FAT 中的条目, 结果放在 ax 中
;	需要注意的是, 中间需要读 FAT 的扇区到 es:bx 处, 所以函数一开始保存了 es 和 bx
; ---------------------------------------------------------------------------
GetFATEntry:
	push	es
	push	bx
	push	ax
	mov	ax, BaseOfKernelFile
	sub	ax, 0100h			; 在 BaseOfKernelFile 后面留出 4 * 1024 空间用于存放 FAT
	mov	es, ax
	pop	ax
	mov	byte [bOdd], 0
	mov	bx, 3
	mul	bx			; dx:ax = ax * 3
	mov	bx, 2
	div	bx			; dx:ax / 2  ==>  ax <- 商, dx <- 余数
	cmp	dx, 0
	jz	LABEL_EVEN
	mov	byte [bOdd], 1
LABEL_EVEN:;偶数
	; 现在 ax 中是 FATEntry 在 FAT 中的偏移量,下面来
	; 计算 FATEntry 在哪个扇区中(FAT占用不止一个扇区)
	xor	dx, dx
	mov	bx, [BPB_BytsPerSec]
	div	bx					; dx:ax / BPB_BytsPerSec
							; ax <- 商 (FATEntry 所在的扇区相对于 FAT 的扇区号)
		   					; dx <- 余数 (FATEntry 在扇区内的偏移)
	push	dx
	mov	bx, 0				; bx <- 0 于是, es:bx = (BaseOfLoader - 100):00
	add	ax, SectorNoOfFAT1	; 此句执行之后的 ax 就是 FATEntry 所在的扇区号
	mov	cl, 2				; 读取 FATEntry 所在的扇区, 一次读两个, 避免在边界发生错误
	call	ReadSector		; 因为一个 FATEntry 可能跨越两个扇区

	pop	dx
	add	bx, dx
	mov	ax, [es:bx]
	cmp	byte [bOdd], 1
	jnz	LABEL_EVEN_2
	shr	ax, 4				; 奇数取高四位（注意对齐方式）
LABEL_EVEN_2:
	and	ax, 0FFFh			; 偶数取低4位（注意对齐方式）

LABEL_GET_FAT_ENRY_OK:
	pop	bx
	pop	es
	ret
;----------------------------------------------------------------------------


;----------------------------------------------------------------------------
; 函数名: KillMotor
;----------------------------------------------------------------------------
; 作用:关闭软驱马达
;----------------------------------------------------------------------------
KillMotor:
	push dx
	mov	dx, 03F2h
	mov	al, 0
	out	dx, al
	pop	dx
	ret
;----------------------------------------------------------------------------


; 从此以后的代码在保护模式下执行 ----------------------------------------------------
; 32 位代码段. 由实模式跳入 ---------------------------------------------------------
[SECTION .s32]

ALIGN	32

[BITS	32]

LABEL_PM_START:
	; 初始化显存地址
	mov	ax, SelectorVideo
	mov	gs, ax
	; 初始化段寄存器和堆栈（这个是从上到下的）
	mov	ax, SelectorFlatRW
	mov	ds, ax
	mov	es, ax
	mov	fs, ax
	mov	ss, ax
	mov	esp, TopOfStack

	push	szMemChkTitle
	call	DispStr
	add	esp, 4

	call	DispMemInfo
	call	SetupPaging

	mov	ah, 0Fh				; 0000: 黑底    1111: 白字
	mov	al, 'P'
	mov	[gs:((80 * 0 + 39) * 2)], ax	; 屏幕第 0 行, 第 39 列
	jmp	$


%include	"lib.inc"


; 显示内存信息 --------------------------------------------------------------
DispMemInfo:
	push	esi
	push	edi
	push	ecx

	mov	esi, MemChkBuf
	mov	ecx, [dwMCRNumber]	
.loop:						; for(int i=0;i<[MCRNumber];i++)	// 每次得到一个ARDS
	mov	edx, 5		 	    ; for(int j=0;j<5;j++)	// 每次得到一个ARDS中的成员
	mov	edi, ARDStruct	    ; 依次显示:BaseAddrLow,BaseAddrHigh,LengthLow,LengthHigh,Type
.1:				 
	push	dword [esi]	  
	call	DispInt		  	; DispInt(MemChkBuf[j*4]) // 显示一个成员
	pop	eax		  
	stosd			 		; ARDStruct[j*4] = MemChkBuf[j*4];
	add	esi, 4		  
	dec	edx
	cmp	edx, 0
	jnz	.1	
	call	DispReturn	  	; printf("\n");
	cmp	dword [dwType], 1   ; if(Type == AddressRangeMemory)
	jne	.2
	mov	eax, [dwBaseAddrLow]
	add	eax, [dwLengthLow]
	cmp	eax, [dwMemSize]    ; if(BaseAddrLow + LengthLow > MemSize)
	jb	.2
	mov	[dwMemSize], eax    ; MemSize = BaseAddrLow + LengthLow;
.2:				  
	loop	.loop
				  
	call	DispReturn	 	 ; printf("\n");
	push	szRAMSize
	call	DispStr		  	 ; printf("RAM size:");
	add	esp, 4
				
	push	dword [dwMemSize] 
	call	DispInt		     ; DispInt(MemSize);
	add	esp, 4

	pop	ecx
	pop	edi
	pop	esi
	ret
; ---------------------------------------------------------------------------

; 启动分页机制 --------------------------------------------------------------
SetupPaging:
	; 根据内存大小计算应初始化多少PDE以及多少页表
	xor	edx, edx
	mov	eax, [dwMemSize]
	mov	ebx, 400000h	; 400000h = 4M = 4096 * 1024, 一个页表对应的内存大小
	div	ebx
	mov	ecx, eax		; 此时 ecx 为页表的个数，也即 PDE 应该的个数
	test	edx, edx
	jz	.no_remainder
	inc	ecx				; 如果余数不为 0 就需增加一个页表
.no_remainder:
	push	ecx			; 暂存页表个数

	; 为简化处理, 所有线性地址对应相等的物理地址. 并且不考虑内存空洞.
	; 首先初始化页目录
	mov	ax, SelectorFlatRW
	mov	es, ax
	mov	edi, PageDirBase	; 此段首地址为 PageDirBase
	xor	eax, eax
	mov	eax, PageTblBase | PG_P  | PG_USU | PG_RWW
.1:
	stosd
	add	eax, 4096			; 为了简化, 所有页表在内存中是连续的
	loop	.1

	; 再初始化所有页表
	pop	eax					; 页表个数
	mov	ebx, 1024			; 每个页表 1024 个 PTE
	mul	ebx
	mov	ecx, eax			; PTE个数 = 页表个数 * 1024
	mov	edi, PageTblBase	; 此段首地址为 PageTblBase
	xor	eax, eax
	mov	eax, PG_P  | PG_USU | PG_RWW
.2:
	stosd
	add	eax, 4096			; 每一页指向 4K 的空间
	loop	.2

	mov	eax, PageDirBase
	mov	cr3, eax
	mov	eax, cr0
	or	eax, 80000000h
	mov	cr0, eax
	jmp	short .3
.3:
	nop

	ret
; 分页机制启动完毕 ----------------------------------------------------------


; SECTION .data1 之开始 ----------------------------------------------------------
[SECTION .data1]

ALIGN	32

LABEL_DATA:
; 实模式下使用这些符号
; 字符串
_szMemChkTitle:	db "BaseAddrL BaseAddrH LengthLow LengthHigh   Type", 0Ah, 0
_szRAMSize:		db "RAM size:", 0
_szReturn:		db 0Ah, 0			; 字符回车
; 变量
_dwMCRNumber:	dd 0				; Memory Check Result
_dwDispPos:		dd (80 * 6 + 0) * 2	; 屏幕第 6 行, 第 0 列
_dwMemSize:		dd 0

_ARDStruct:	; Address Range Descriptor Structure
	_dwBaseAddrLow:		dd	0
	_dwBaseAddrHigh:	dd	0
	_dwLengthLow:		dd	0
	_dwLengthHigh:		dd	0
	_dwType:			dd	0

_MemChkBuf:	 times 256	db	0

; 保护模式下使用这些符号（保护模式是靠段选择子寻址，所以这里用物理地址寻址）
szMemChkTitle	equ		BaseOfLoaderPhyAddr + _szMemChkTitle
szRAMSize		equ		BaseOfLoaderPhyAddr + _szRAMSize
szReturn		equ		BaseOfLoaderPhyAddr + _szReturn
dwDispPos		equ		BaseOfLoaderPhyAddr + _dwDispPos
dwMemSize		equ		BaseOfLoaderPhyAddr + _dwMemSize
dwMCRNumber		equ		BaseOfLoaderPhyAddr + _dwMCRNumber
ARDStruct		equ		BaseOfLoaderPhyAddr + _ARDStruct
dwBaseAddrLow	equ		BaseOfLoaderPhyAddr + _dwBaseAddrLow
dwBaseAddrHigh	equ		BaseOfLoaderPhyAddr + _dwBaseAddrHigh
dwLengthLow		equ		BaseOfLoaderPhyAddr + _dwLengthLow
dwLengthHigh	equ		BaseOfLoaderPhyAddr + _dwLengthHigh
dwType			equ		BaseOfLoaderPhyAddr + _dwType
MemChkBuf		equ		BaseOfLoaderPhyAddr + _MemChkBuf


; 堆栈就在数据段的末尾
StackSpace:	times	1024	db	0
; 栈顶
TopOfStack	equ	BaseOfLoaderPhyAddr + $	
; SECTION .data1 之结束 ----------------------------------------------------------