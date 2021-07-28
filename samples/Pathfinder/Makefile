VPATH = src

all: main

main: main.adb
	gprbuild

.PHONY: clean
clean:
	rm -rf build/* src/*~
	gprclean
