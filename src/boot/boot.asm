;*******************************
; FileName: src/boot/boot.asm
; Author: WenShiHuai
; CreateDate: 2020/7/27
; Describe: 引导程序
;*******************************

    org 0x7c00       ; 加载程序到内存-----7c00-----处,ORG是伪指令 偏移地址CS:IP=0000:7C00

;----------------------------------------------------------------------------
BaseOfStack				equ	07c00h
;----------------------------------------------------------------------------

%include	"load.inc"

	jmp short LABEL_START		; Start to boot.
	nop

; 下面是 FAT12 磁盘的头, 之所以包含它是因为下面用到了磁盘的一些信息
%include	"fat12hdr.inc"

LABEL_START:
    mov ax, cs       ; 初始化代码段和数据段
    mov ds, ax
    mov es, ax
    mov	ss, ax
	mov	sp, BaseOfStack

	mov ax, 0x0600	 ; 清屏
	mov bx, 0x0700
	mov cx, 0
	mov dx, 0x0184f
	int 0x10

	mov dh, 0		 ; 打印"Loading system ..."
    call	DispStr  ; 调用显示字符串例程

    xor	ah, ah		 ; 软驱复位
	xor	dl, dl	     
	int	13h

	; 在软盘的根目录寻找 LOADER.BIN
	; 初始化扇区查找的开始区域
	mov	word [wSectorNo], SectorNoOfRootDirectory	; SectorNoOfRootDirectory根目录开始扇区号

LABEL_SEARCH_IN_ROOT_DIR_BEGIN:
	cmp	word [wRootDirSizeForLoop], 0		; 判断根目录区是不是已经读完
	jz	LABEL_NO_LOADERBIN					; 如果读完表示没有找到 LOADER.BIN,跳转到没有找到

	dec	word [wRootDirSizeForLoop]			; 查找扇区向前移动一位
	mov	ax, BaseOfLoader
	mov	es, ax				; es <- BaseOfLoader
	mov	bx, OffsetOfLoader	; bx <- OffsetOfLoader	于是, es:bx = BaseOfLoader:OffsetOfLoader
	mov	ax, [wSectorNo]		; ax <- Root Directory 中的某 Sector 号

	mov	cl, 1				; 连续读取的扇区数
	call	ReadSector

	mov	si, LoaderFileName	; ds:si -> "LOADER  BIN"
	mov	di, OffsetOfLoader	; es:di -> BaseOfLoader:0100 = BaseOfLoader*10h+100
	cld						; 使字符串操作中SI或DI地址指针自动增加，字符串处理由前往后
	mov	dx, 10h				; 循环的次数 = 512 / 32个文件目录项	

LABEL_SEARCH_FOR_LOADERBIN:	; 寻找loader.bin文件
	cmp	dx, 0								; 循环次数控制,
	jz	LABEL_GOTO_NEXT_SECTOR_IN_ROOT_DIR	; 如果已经读完了一个 Sector,就跳到下一个 Sector
	dec	dx
	mov	cx, 11		; 查找文件的文件名长度

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
	and	di, 0FFE0h					; 1111 1111 1110 0000 di &= E0 为了让它指向本条目开头
	add	di, 20h						; 文件目录占32字节 di += 20h  下一个目录条目
	mov	si, LoaderFileName
	jmp	LABEL_SEARCH_FOR_LOADERBIN

LABEL_GOTO_NEXT_SECTOR_IN_ROOT_DIR:
	add	word [wSectorNo], 1
	jmp	LABEL_SEARCH_IN_ROOT_DIR_BEGIN

LABEL_NO_LOADERBIN:
	mov	dh, 2			; "No LOADER."
	call	DispStr		; 显示字符串
	jmp $

LABEL_FILENAME_FOUND:		; 找到 LOADER.BIN，继续找文件数据
	mov	ax, RootDirSectors	; 根目录大小
	and	di, 0FFE0h			; di -> 当前条目的开始
	add	di, 01Ah			; 根目录偏移，此条目对应的开始簇号
	mov	cx, word [es:di]
	push	cx				; 保存此 Sector 在 FAT 中的簇号(只有簇号信息 >= 0x2h)
	add	cx, ax
	add	cx, DeltaSectorNo	; cl <- LOADER.BIN的起始扇区号(逻辑)
	mov	ax, BaseOfLoader
	mov	es, ax				; es <- BaseOfLoader
	mov	bx, OffsetOfLoader	; bx <- OffsetOfLoader
	mov	ax, cx				; ax <- Sector 号（逻辑扇区号）

LABEL_GOON_LOADING_FILE:
	push	ax
	push	bx
	mov	ah, 0Eh				; 每读一个扇区就在 "Loading System  " 后面
	mov	al, '.'				; 打一个点, 形成这样的效果: ...
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
	mov	dh, 1				; "Ready To Load ... "
	call	DispStr			; 显示字符串

; ----------------------------------------------------------------------------
; 这一句正式跳转到已加载到内存中的 LOADER.BIN 的开始处，开始执行 LOADER.BIN 的代码。
; ----------------------------------------------------------------------------
	jmp	BaseOfLoader:OffsetOfLoader	


;----------------------------------------------------------------------------
;变量
;----------------------------------------------------------------------------
wRootDirSizeForLoop	dw	RootDirSectors		; Root Directory 占用的扇区数, 在循环中会递减至零.
wSectorNo			dw	0					; 要读取的扇区号
bOdd				db	0					; 奇数还是偶数

;----------------------------------------------------------------------------
;字符串
;----------------------------------------------------------------------------
LoaderFileName		db	"LOADER  BIN", 0	; LOADER.BIN 之文件名
; 为简化代码, 下面每个字符串的长度均为 MessageLength
MessageLength	equ	18
BootMessage:    db  "Booting System    "	; 18字节, 不够则用空格补齐. 序号 0
Message1		db	"Ready To Load ... "	; 18字节, 不够则用空格补齐. 序号 1
Message2		db	"No Loader File ..."	; 18字节, 不够则用空格补齐. 序号 2

;----------------------------------------------------------------------------
; 函数名: DispStr
;----------------------------------------------------------------------------
; 作用:	通过dh确定字符串位置，打印字符串
; ---------------------------------------------------------------------------
DispStr:
	mov	ax, MessageLength
	mul	dh
	add	ax, BootMessage		; 字符串偏移地址
	mov	bp, ax				; 字符串地址放入ES：BP
	mov	ax, ds
	mov	es, ax
	mov	cx, MessageLength	; CX = 串长度
	mov	ax, 01301h			; AH = 13,  AL = 01h
	mov	bx, 0007h			; 页号为0(BH = 0) 黑底白字(BL = 07h)
	mov	dl, 0
	int	10h					; int 10h
	ret
; ---------------------------------------------------------------------------


;----------------------------------------------------------------------------
; 函数名: ReadSector
;----------------------------------------------------------------------------
; 作用:	从第 ax 个 Sector 开始, 将 cl 个 Sector 读入 es:bx 中
; ---------------------------------------------------------------------------
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


; ---------------------------------------------------------------------------
; 函数名: GetFATEntry
;----------------------------------------------------------------------------
; 作用:	找到序号为 ax 的 Sector 在 FAT 中的条目, 结果放在 ax 中
;	   需要注意的是, 中间需要读 FAT 的扇区到 es:bx 处, 所以函数一开始保存了 es 和 bx
; ---------------------------------------------------------------------------
GetFATEntry:
	push	es
	push	bx
	push	ax
	mov	ax, BaseOfLoader
	sub	ax, 0100h		; 在 BaseOfLoader 后面留出 4 * 1024 空间用于存放 FAT
	mov	es, ax
	pop	ax
	mov	byte [bOdd], 0
	mov	bx, 3
	mul	bx				; dx:ax = ax * 3
	mov	bx, 2
	div	bx				; dx:ax / 2  ==>  ax <- 商, dx <- 余数
	cmp	dx, 0
	jz	LABEL_EVEN
	mov	byte [bOdd], 1
LABEL_EVEN:;偶数
	; 现在 ax 中是 FATEntry 在 FAT 中的偏移量,下面来
	; 计算 FATEntry 在哪个扇区中(FAT占用不止一个扇区)
	xor	dx, dx			
	mov	bx, [BPB_BytsPerSec]
	div	bx 				   	; dx:ax / BPB_BytsPerSec
		   				   	; ax <- 商 (FATEntry 所在的扇区相对于 FAT 的扇区号)
		   				  	; dx <- 余数 (FATEntry 在扇区内的偏移)
	push	dx
	mov	bx, 0 			 	; bx <- 0 于是, es:bx = (BaseOfLoader - 100):00
	add	ax, SectorNoOfFAT1 	; 此句之后的 ax 就是 FATEntry 所在的扇区号
	mov	cl, 2				; 读取 FATEntry 所在的扇区, 一次读两个, 避免在边界发生错误
	call	ReadSector 		; 因为一个 FATEntry 可能跨越两个扇区
			  
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

times   510 - ($ - $$)    db 	0      ; 填充剩下的空间，使生成的二进制代码恰好为512字节
db  0x55
db  0xAA    ; 结束标志