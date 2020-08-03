##################################################
# Makefile
##################################################

BOOT:=boot.asm
LDR:=loader.asm
# subst是字符串替换函数
# 第一个参数是被替换字串，第二个参数是替换字串，第三个参数是目标字符串。

BOOT_BIN:=$(subst .asm,.bin,$(BOOT))
LDR_BIN:=$(subst .asm,.bin,$(LDR))

IMG:=wetree.img
FLOPPY:=/mnt/floppy/

.PHONY : everything

everything : $(BOOT_BIN) $(LDR_BIN)
	dd if=$(BOOT_BIN) of=$(IMG) bs=512 count=1 conv=notrunc
	sudo mount -o loop $(IMG) $(FLOPPY)
	sudo cp $(LDR_BIN) $(FLOPPY) -v
	sudo umount $(FLOPPY)

clean :
	rm -f $(BOOT_BIN) $(LDR_BIN) *.o

# $@ 目标文件，$^ 所有的依赖文件，$< 第一个依赖文件
$(BOOT_BIN) : $(BOOT)
	nasm $< -o $@

$(LDR_BIN) : $(LDR)
	nasm $< -o $@

