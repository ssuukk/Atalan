ATLDESTS= fifteen.xex test.xex

all: $(ATLDESTS)

fifteen.xex: fifteen.asm
	mads $< -i:../../src -o:$@ -x
%.bin: %.asm
	mads $< -p:a2600 -i:../../src -o:$@ -x

%.xex: %.asm
	mads $< -i:../../src -o:$@ -x

%.asm: %.atl
	../../src/atalan  -a -I ../../src $<

clean:
	rm -fv *.xex *.asm

