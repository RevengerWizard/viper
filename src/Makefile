##############################################################################
# Viper Makefile. Requires GNU Make.
##############################################################################

CC = clang -fsanitize=address

CCOPT = #-O2

CCDEBUG = -O0 -g3

CCWARN = -Wall -Wextra

XCFLAGS = -DVIPER_USE_ASSERT

##############################################################################
# Host system detection
##############################################################################

ifeq (Windows,$(findstring Windows,$(OS))$(MSYSTEM)$(TERM))
	HOST_SYS= Windows
else
	HOST_SYS:= $(shell uname -s)
	ifneq (,$(findstring MINGW,$(HOST_SYS)))
		HOST_SYS= Windows
		HOST_MSYS= mingw
	endif
	ifneq (,$(findstring MSYS,$(HOST_SYS)))
		HOST_SYS= Windows
		HOST_MSYS= mingw
	endif
	ifneq (,$(findstring CYGWIN,$(HOST_SYS)))
		HOST_SYS= Windows
		HOST_MSYS= cygwin
	endif
endif

##############################################################################
# Flags and options for host and target
##############################################################################

ASOPTIONS = $(CCOPT) $(CCWARN) $(XCFLAGS) $(CFLAGS)
CCOPTIONS = $(CCDEBUG) $(ASOPTIONS)
LDOPTIONS = $(CCDEBUG) $(LDFLAGS)

HOST_CC = $(CC)
HOST_RM ?= rm -f

HOST_XCFLAGS = -I.
HOST_XLDFLAGS =
HOST_XLIBS =
HOST_ACFLAGS = $(CCOPTIONS) $(HOST_XCFLAGS) $(TARGET_ARCH) $(HOST_CFLAGS)
HOST_ALDFLAGS = $(LDOPTIONS) $(HOST_XLDFLAGS) $(HOST_LDFLAGS)
HOST_ALIBS = $(HOST_XLIBS) $(LIBS) $(HOST_LIBS)

TARGET_LD = $(CC)
TARGET_AR = ar rcu
TARGET_STRIP = strip

TARGET_LFSFLAGS = -D_FILE_OFFSET_BITS=64 -D_LARGEFILE_SOURCE
TARGET_XCFLAGS = $(TARGET_LFSFLAGS) -U_FORTIFY_SOURCE
TARGET_XLDFLAGS =
TARGET_XLIBS = #-lm
TARGET_ACFLAGS = $(CCOPTIONS) $(TARGET_XCFLAGS) $(TARGET_FLAGS) $(TARGET_CFLAGS)
TARGET_ASFLAGS = $(ASOPTIONS) $(TARGET_XCFLAGS) $(TARGET_FLAGS) $(TARGET_CFLAGS)
TARGET_ALDFLAGS = $(LDOPTIONS) $(TARGET_XLDFLAGS) $(TARGET_FLAGS) $(TARGET_LDFLAGS)
TARGET_ALIBS = $(TARGET_XLIBS) $(LIBS) $(TARGET_LIBS)

##############################################################################
# Target system detection
##############################################################################

TARGET_SYS ?= $(HOST_SYS)
ifeq (Windows,$(TARGET_SYS))
	TARGET_STRIP += --strip-unneeded
	VP_T = vxc.exe
else
	TARGET_AR += 2>/dev/null
	ifeq (,$(shell $(CC) -o /dev/null -c -x c /dev/null -fno-stack-protector 2>/dev/null || echo 1))
		TARGET_XCFLAGS += -fno-stack-protector
	endif
	VP_T = vxc
	ifeq (Linux,$(TARGET_SYS))
		TARGET_XLIBS += -ldl
	endif
	ifeq (GNU/kFreeBSD,$(TARGET_SYS))
		TARGET_XLIBS += -ldl
	endif
	ifneq (SunOS,$(TARGET_SYS))
		ifneq (PS3,$(TARGET_SYS))
			TARGET_XLDFLAGS += -Wl,-E
		endif
	endif
endif

ifneq (,$(CCDEBUG))
	TARGET_STRIP = @:
endif

##############################################################################
# Files and pathnames
##############################################################################

VPCORE_O = vp_assert.o vp_mem.o vp_buf.o \
	vp_asm_x64.o \
	vp_target.o \
	vp_str.o vp_vec.o vp_tab.o vp_map.o vp_type.o \
	vp_char.o vp_strscan.o \
	vp_lex.o vp_parse.o vp_ast.o \
	vp_load.o vp_sema.o vp_err.o \
	vp_opt.o \
	vp_codegen.o vp_ir.o vp_var.o vp_regalloc.o \
	vp_sel_x64.o \
	vp_dump.o vp_state.o

VP_O = viper.o
VP_A = libvp.a

ALL_T = $(VP_T) $(VP_A)
WIN_RM = *.obj *.lib *.exp *.dll *.exe *.manifest *.pdb *.ilk
ALL_RM = $(ALL_T) *.o $(WIN_RM)

##############################################################################

ifeq (Windows,$(HOST_SYS))
	ifeq (,$(HOST_MSYS))
		ALL_RM := $(subst /,\,$(ALL_RM))
		HOST_RM = del
	endif
endif

Q = @
E = @echo
#Q =
#E = @:

##############################################################################
# Make targets
##############################################################################

default all: $(VP_T)

clean:
	$(HOST_RM) $(ALL_RM)

depend:
	$(Q)$(HOST_CC) $(HOST_ACFLAGS) -MM *.c > Makefile.dep

.PHONY: default all amalg clean depend

##############################################################################
# Object file rules
##############################################################################

%.o: %.c
	$(E) "CC        $@"
	$(Q)$(CC) $(TARGET_ACFLAGS) -c -o $@ $<

$(VP_O):
	$(E) "CC        $@"
	$(Q)$(CC) $(TARGET_ACFLAGS) -c -o $@ $<

include Makefile.dep

##############################################################################
# Target file rules
##############################################################################

$(VP_A): $(VPCORE_O)
	$(E) "AR        $@"
	$(Q)$(TARGET_AR) $@ $(VPCORE_O)

$(VP_T): $(VP_A) $(VP_O)
	$(E) "LINK      $@"
	$(Q)$(TARGET_LD) $(TARGET_ALDFLAGS) -o $@ $(VP_O) $(VP_A) $(TARGET_ALIBS)
	$(Q)$(TARGET_STRIP) $@
	$(E) "OK        Successfully built Viper"

##############################################################################