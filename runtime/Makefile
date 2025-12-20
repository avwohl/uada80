# Makefile for libada.lib - Ada runtime library for Z80
# Uses um80 assembler and ulib80 library manager
#
# Usage:
#   make            - Build for CP/M (default, uses tasking.mac)
#   make TARGET=mpm - Build for MP/M II (uses mpm_task.mac)
#   make clean      - Remove built files
#   make rebuild    - Clean and rebuild

# Target selection: cpm (default) or mpm
TARGET ?= cpm

# Path to um80 tools
UM80_PATH = /home/wohl2/src/um80_and_friends
UM80 = PYTHONPATH=$(UM80_PATH) python3 -m um80.um80
ULIB80 = PYTHONPATH=$(UM80_PATH) python3 -m um80.ulib80

# Common source files (non-tasking)
# NOTE: protlock.mac MUST be before tasking.mac to provide _PROT_LCK/_PROT_ULK
COMMON_SOURCES = runtime.mac containers.mac vectors.mac lists.mac fileio.mac float48.mac float64.mac timer.mac protlock.mac

# Tasking source - depends on target
ifeq ($(TARGET),mpm)
TASKING_SOURCE = mpm_task.mac
LIBRARY = libada_mpm.lib
else
TASKING_SOURCE = tasking.mac
LIBRARY = libada.lib
endif

# All macro assembler sources
MAC_SOURCES = $(COMMON_SOURCES) $(TASKING_SOURCE)
MAC_OBJECTS = $(MAC_SOURCES:.mac=.rel)

# Pre-compiled object files (no source available)
PREBUILT_OBJECTS = strings.rel bounded.rel heap.rel math.rel io.rel

OBJECTS = $(PREBUILT_OBJECTS) $(MAC_OBJECTS)

.PHONY: all clean rebuild mpm cpm

all: $(LIBRARY)

# Convenience targets
cpm:
	$(MAKE) TARGET=cpm

mpm:
	$(MAKE) TARGET=mpm

rebuild: clean all

$(LIBRARY): $(OBJECTS)
	$(ULIB80) -c $@ $(OBJECTS)

%.rel: %.mac
	$(UM80) -o $@ $<

clean:
	rm -f $(MAC_OBJECTS) libada.lib libada_mpm.lib mpm_task.rel protlock.rel

# Dependencies for .mac source files
runtime.rel: runtime.mac
containers.rel: containers.mac
vectors.rel: vectors.mac
lists.rel: lists.mac
tasking.rel: tasking.mac
mpm_task.rel: mpm_task.mac
fileio.rel: fileio.mac
float48.rel: float48.mac
float64.rel: float64.mac
timer.rel: timer.mac
protlock.rel: protlock.mac
