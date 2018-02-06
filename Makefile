export LC_ALL = C

SYSTEM_NAME := $(shell uname -s)
SYSTEM_ARCH := $(shell uname -m)

###### generic rules #######
all:	atalan

atalan:
	@echo System name: $(SYSTEM_NAME)
	@echo System Arch: $(SYSTEM_ARCH)
	cd src/common; make
	cd src/atalan; make

clean:
	cd src/common; make clean
	cd src/atalan; make clean
	cd examples; make clean

examples:
	cd examples; make all

