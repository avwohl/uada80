# Makefile for libada.lib - Ada runtime library for Z80
# Uses um80 assembler and ulib80 library manager

# Path to um80 tools
UM80_PATH = /home/wohl2/src/um80_and_friends
UM80 = PYTHONPATH=$(UM80_PATH) python3 -m um80.um80
ULIB80 = PYTHONPATH=$(UM80_PATH) python3 -m um80.ulib80

SOURCES = strings.asm bounded.asm containers.asm heap.asm math.asm io.asm tasking.asm fileio.asm float48.asm timer.asm
OBJECTS = $(SOURCES:.asm=.rel)

LIBRARY = libada.lib

.PHONY: all clean

all: $(LIBRARY)

$(LIBRARY): $(OBJECTS)
	$(ULIB80) -c $@ $(OBJECTS)

%.rel: %.asm
	$(UM80) -o $@ $<

clean:
	rm -f $(OBJECTS) $(LIBRARY)

# Dependencies
strings.rel: strings.asm
containers.rel: containers.asm
heap.rel: heap.asm
math.rel: math.asm
io.rel: io.asm
tasking.rel: tasking.asm
fileio.rel: fileio.asm
float48.rel: float48.asm
timer.rel: timer.asm
