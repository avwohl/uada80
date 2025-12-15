# Makefile for libada.lib - Ada runtime library for Z80
# Uses um80 assembler and ulib80 library manager

# Path to um80 tools
UM80_PATH = /home/wohl2/src/um80_and_friends
UM80 = PYTHONPATH=$(UM80_PATH) python3 -m um80.um80
ULIB80 = PYTHONPATH=$(UM80_PATH) python3 -m um80.ulib80

# Source files with .mac extension (macro assembler source)
MAC_SOURCES = runtime.mac containers.mac vectors.mac lists.mac tasking.mac fileio.mac float48.mac timer.mac
MAC_OBJECTS = $(MAC_SOURCES:.mac=.rel)

# Pre-compiled object files (no source available)
PREBUILT_OBJECTS = strings.rel bounded.rel heap.rel math.rel io.rel

OBJECTS = $(PREBUILT_OBJECTS) $(MAC_OBJECTS)

LIBRARY = libada.lib

.PHONY: all clean

all: $(LIBRARY)

$(LIBRARY): $(OBJECTS)
	$(ULIB80) -c $@ $(OBJECTS)

%.rel: %.mac
	$(UM80) -o $@ $<

clean:
	rm -f $(MAC_OBJECTS) $(LIBRARY)

# Dependencies for .mac source files
runtime.rel: runtime.mac
containers.rel: containers.mac
vectors.rel: vectors.mac
lists.rel: lists.mac
tasking.rel: tasking.mac
fileio.rel: fileio.mac
float48.rel: float48.mac
timer.rel: timer.mac
