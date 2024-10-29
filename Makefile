
H = ../../Z80_dongle/Z80_dongle/basic1K.h
HEX = basic1K.hex basic1K8080.hex
ROMSIZE = 0x0400


.PHONY:	all
all: $(H) $(HEX)


.PHONY:	clean
clean:
	rm *.cim *.lst *.hex

$(H): basic1K.cim obj2h
	./obj2h $< $(ROMSIZE) rom_b1K > $@

basic1K.hex: basic1K.cim
	srec_cat $< -binary -output $@ -intel --address-length=2

basic1K8080.hex: basic8080.hex
	srec_cat $< -intel -output $@ -intel --address-length=2

basic1K.cim: basic1K.asm Makefile
	zmac -c -o $@ -o basic1K.lst $<

obj2h: obj2h.cpp
	gcc -Wall -o $@ $<
