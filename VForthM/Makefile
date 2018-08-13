CC=gcc
LIBS = -ldl -lreadline

all: LinuxSPF32VM

LinuxSPF32VM:  tmp/LinuxSPF32VM.o tmp/forth.o tmp/iovfm.o
	$(CC) -o  LinuxSPF32VM tmp/LinuxSPF32VM.o tmp/forth.o tmp/iovfm.o $(LIBS) -Wl,-Map,tmp/messy.map
	objdump -h -S -C -D -r LinuxSPF32VM > tmp/LinuxSPF32VM.lss

tmp/LinuxSPF32VM.o: LinuxSPF32VM.c
	$(CC) -c -g LinuxSPF32VM.c -o $@
	echo $(CC) -c -g -m64 LinuxSPF32VM.c -o $@
	objdump -x -S $@ > tmp/LinuxSPF32VM.lss

tmp/forth.o: forth.c 
	$(CC) -c -g forth.c -o $@

tmp/iovfm.o: iovfm.c 
	$(CC) -c -g iovfm.c -o $@

clean:
	-rm -f LinuxSPF32VM
	-rm -f tmp/*
