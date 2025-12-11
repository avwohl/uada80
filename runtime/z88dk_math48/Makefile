# Makefile for z88dk math48 floating-point library
# Converted from z88dk sources for use with um80/ulib80

# Path to um80 tools
UM80_PATH = /home/wohl/src/um80_and_friends
UM80 = PYTHONPATH=$(UM80_PATH) python3 -m um80.um80
ULIB80 = PYTHONPATH=$(UM80_PATH) python3 -m um80.ulib80

# All source files
SOURCES = \
	am48_support.asm \
	mm48__ac1.asm \
	mm48__ac1_3.asm \
	mm48__acln2.asm \
	mm48__acpi.asm \
	mm48__acsqr2_2.asm \
	mm48__addac.asm \
	mm48__calcs.asm \
	mm48__cmpac.asm \
	mm48__comser.asm \
	mm48__expsgn.asm \
	mm48__getncix.asm \
	mm48__sleft.asm \
	mm48__sright.asm \
	mm48__subac.asm \
	mm48__zero.asm \
	mm48_atn.asm \
	mm48_cmp.asm \
	mm48_cos.asm \
	mm48_equal.asm \
	mm48_exp.asm \
	mm48_fpadd.asm \
	mm48_fpdiv.asm \
	mm48_fpmul.asm \
	mm48_fpsub.asm \
	mm48_frac.asm \
	mm48_int.asm \
	mm48_ln.asm \
	mm48_log.asm \
	mm48_mod.asm \
	mm48_mul10.asm \
	mm48_negate.asm \
	mm48_pwr.asm \
	mm48_sin.asm \
	mm48_sqr.asm \
	mm48_tan.asm \
	mm48_tenf.asm

OBJECTS = $(SOURCES:.asm=.rel)

LIBRARY = libmath48.lib

.PHONY: all clean test

all: $(LIBRARY)

$(LIBRARY): $(OBJECTS)
	$(ULIB80) -c $@ $(OBJECTS)

%.rel: %.asm
	$(UM80) -o $@ $<

# Test assembly of all files (no linking)
test:
	@echo "Testing assembly of all files..."
	@for f in $(SOURCES); do \
		echo "  $$f"; \
		$(UM80) -o $${f%.asm}.rel $$f || exit 1; \
	done
	@echo "All files assembled successfully."

clean:
	rm -f $(OBJECTS) $(LIBRARY)
