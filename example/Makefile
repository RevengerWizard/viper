VXC ?= vxc
LD ?= ld

VXC_DIR := $(patsubst %/,%,$(dir $(abspath $(shell command -v $(VXC)))))
STD_OBJS := $(wildcard $(VXC_DIR)/std/*.obj)

EXES := cube.exe triangle.exe
LDLIBS := -lglfw3 -lkernel32 -luser32 -lgdi32 -lopengl32
LDFLAGS := --subsystem=console -e main

all: cube

cube: cube.exe
triangle: triangle.exe

%.exe: %.vp
	$(VXC) comp $<
	$(LD) $*.obj $(STD_OBJS) -o $@ $(LDLIBS) $(LDFLAGS)

clean:
	$(RM) -f $(EXES) *.obj

.PHONY: all cube triangle clean
