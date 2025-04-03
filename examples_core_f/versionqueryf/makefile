#===============================================================================
# Copyright 2008-2020 Intel Corporation.
#
# This software and the related documents are Intel copyrighted  materials,  and
# your use of  them is  governed by the  express license  under which  they were
# provided to you (License).  Unless the License provides otherwise, you may not
# use, modify, copy, publish, distribute,  disclose or transmit this software or
# the related documents without Intel's prior written permission.
#
# This software and the related documents  are provided as  is,  with no express
# or implied  warranties,  other  than those  that are  expressly stated  in the
# License.
#===============================================================================

##  Content:
##      Intel(R) Math Kernel Library. versionquery example creation and run
##******************************************************************************

help:
	@echo $$'$(WRONG_OPTION)'
	@echo "Intel(R) MKL versionquery example help."
	@echo ""
	@echo "Usage: make Target [Options...]"
	@echo ""
	@echo "   Target:"
	@echo "       libintel64   - statically linked example for processors that use the Intel(R) 64 architecture"
	@echo "       dylibintel64 - dynamically linked example for processors that use the Intel(R) 64 architecture"
	@echo "       help         - print this help"
	@echo ""
	@echo "   Options:"
	@echo "       interface={lp64|ilp64}"
	@echo "           programming interface for intel64."
	@echo "           Default: lp64."
	@echo "       threading={parallel|sequential}"
	@echo "           Specifies whether to use Intel(R) MKL in the threaded or sequential mode."
	@echo "           Default: parallel."
	@echo "       MKLROOT=<MKL_directory>"
	@echo "           Specifies the location of Intel(R) MKL libraries used to build this example."
	@echo "           Default: the Intel(R) MKL installation directory."
	@echo ""
	@echo "Usage examples:"
	@echo ""
	@echo "   make dylibintel64 interface=ilp64 threading=sequential"
	@echo "       Link 'versionquery' example against sequential dynamic MKL-Intel64"
	@echo "       using ILP64 interface."
	@echo ""
#------------------------------------------------------------------------------

FUNCTION_LIST = getversion
TARGET = $(FUNCTION_LIST)

# Defaults
compiler=intel
interface=lp64
threading=parallel

ifneq ($(compiler),intel)
MSG2+= compiler=$(compiler)
endif

ifneq ($(interface),ilp64)
ifneq ($(interface),lp64)
MSG2+= interface=$(interface)
endif
endif

ifneq ($(threading),parallel)
ifneq ($(threading),sequential)
MSG2+= threading=$(threading)
endif
endif

ifneq ("$(MSG2)","")
WRONG_OPTION=\n\n*** COMMAND LINE ERROR: Wrong value of option(s):  $(MSG2)\n\n
TARGET=help
endif
###

ifdef _IA

ifeq ($(SD),static)
  EXT=a
  RES_EXT=lib
else
  EXT=dylib
  RES_EXT=dylib
endif

ifndef MKLROOT
MKLROOT = ../..
endif
MKL_PATH = $(MKLROOT)/lib
CMPLR_PATH = $(MKLROOT)/../compiler/lib

override COMPILER = ifort
OPTIONS = -fpp -warn all -warn errors
IFACE_COMP_PART = intel
IFACE_THREADING_PART = intel

IFACE_SUFF=_$(interface)
M64 = -m64 # This option tells compiler to generate code for Intel64 architecture.

IFACE_LIB="$(MKL_PATH)/libmkl_$(IFACE_COMP_PART)$(IFACE_SUFF).$(EXT)"

ifeq ($(interface),ilp64)
  OPTIONS += -i8
endif

ifeq ($(threading),sequential)
  THREADING_LIB="$(MKL_PATH)/libmkl_sequential.$(EXT)"
  OMP_LIB =
else
  THREADING_LIB="$(MKL_PATH)/libmkl_$(IFACE_THREADING_PART)_thread.$(EXT)"
  OMP_LIB = -L"$(CMPLR_PATH)" -liomp5
endif

RPATH = -Wl,-rpath,$(MKL_PATH) -Wl,-rpath,$(CMPLR_PATH)
CORE_LIB=$(MKL_PATH)/libmkl_core.$(EXT)

LABEL = $(compiler)$(IFACE_SUFF)_$(threading)_$(_IA)_$(RES_EXT)
RES_DIR = _results/$(LABEL)

ifneq ("$(out_to_screen)","")
OUT_TO_FILE=
else
OUT_TO_FILE=> $(RES_DIR)/$@.res
endif

endif # ifdef _IA

#-------------------------------------------------------------------------------

intel64: libintel64 dylibintel64

libintel64 libem64t:
	@$(MAKE) $(TARGET) --no-print-directory SD=static  _IA=intel64
dylibintel64 dylibem64t:
	@$(MAKE) $(TARGET) --no-print-directory SD=dynamic _IA=intel64

.PHONY: clean
clean:
	-rm -rf _results;

#-------------------------------------------------------------------------------

$(FUNCTION_LIST):
	@mkdir -p ./$(RES_DIR)
	@echo $$'\n----- Compiling $(LABEL) ----- $@'
	$(COMPILER) $(M64) $(OPTIONS) -I"$(MKLROOT)/include" \
	./source/$@.f \
	$(IFACE_LIB) \
	$(THREADING_LIB) \
	$(RPATH) \
	$(CORE_LIB) \
	$(OMP_LIB) -lpthread -lm -o $(RES_DIR)/$@.out
	@echo ----- Execution $(LABEL) ----- $@
	$(RES_DIR)/$@.out $(OUT_TO_FILE)

#-------------------------------------------------------------------------------
