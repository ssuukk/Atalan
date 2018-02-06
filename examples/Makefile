ATLDESTS= count_2sec.xex esieve.xex hello_font.xex hello_world.xex interrupts.xex loop.xex rainbow.xex snake.xex stars.xex tetris.xex unit_test.xex 

all: $(ATLDESTS)

%.xex: %.asm
	mads $< -i:../src -o:$@ -x

%.asm: %.atl
	../src/atalan -a -I ../src $<

clean:
	rm -fv *.xex *.asm

