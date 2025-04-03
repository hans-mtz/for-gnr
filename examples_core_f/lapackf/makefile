#===============================================================================
# Copyright 2005-2020 Intel Corporation.
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
##      Intel(R) Math Kernel Library LAPACK examples creation and run
##******************************************************************************

help:
	@echo "Usage: make {libintel64|dylibintel64} [function=name]"
	@echo "[interface=interface_name] [threading=threading_name]"
	@echo "name     - function name. Please see lapack.lst file"
	@echo "interface_name    - can be lp64 or ilp64 for intel64. Default value is lp64."
	@echo "threading_name    - can be omp, tbb or sequential. Default value is omp."

##------------------------------------------------------------------------------
## examples of using:
##
## make libintel64 function=dgebrd
##     build  by  Intel(R) Fortran Compiler  (as default)
##     and run DGEBRD example for 64-bit applications, static linking
##------------------------------------------------------------------------------

include lapack.lst

ifndef function
   function = $(LAPACK)
endif

ifndef compiler
compiler = intel
endif

ifndef interface
interface = lp64
endif

ifneq ($(threading),sequential)
   ifneq ($(threading),tbb)
      override threading=omp
   endif
endif

ifndef MKLROOT
MKLROOT = ../..
endif
MKL_PATH = $(MKLROOT)/lib
CMPLR_PATH = $(MKLROOT)/../compiler/lib
TBB_PATH = $(MKLROOT)/../tbb/lib

override FC=ifort
IFACE_COMP_PART = intel
IFACE_THREADING_PART = intel

ifeq ($(interface),ilp64)
IFACE_LIB = $(MKL_PATH)/libmkl_$(IFACE_COMP_PART)_ilp64.$(EXT)
FOPTS = -i8
else
IFACE_LIB = $(MKL_PATH)/libmkl_$(IFACE_COMP_PART)_lp64.$(EXT)
FOPTS =
endif
FOPTS += -warn all -warn errors

ifeq ($(threading),sequential)
   THREAD_LIB = $(MKL_PATH)/libmkl_sequential.$(EXT)
   LL_THREAD_LIB =
else
   ifeq ($(threading),omp)
      THREAD_LIB = $(MKL_PATH)/libmkl_$(IFACE_THREADING_PART)_thread.$(EXT)
      LL_THREAD_LIB = -L$(CMPLR_PATH) -liomp5
   else
      THREAD_LIB = $(MKL_PATH)/libmkl_tbb_thread.$(EXT)
      LL_THREAD_LIB.a     = -L$(TBB_PATH) -ltbb -lc++
      LL_THREAD_LIB.dylib =
      LL_THREAD_LIB      := $(LL_THREAD_LIB.$(EXT))
   endif
endif

RPATH = -Wl,-rpath,$(MKL_PATH) -Wl,-rpath,$(CMPLR_PATH) -Wl,-rpath,$(TBB_PATH)
RES = $(addsuffix x.res ,$(function))

CORE_LIB = $(MKL_PATH)/libmkl_core.$(EXT)

MKL_LIBS = $(IFACE_LIB) $(THREAD_LIB) $(CORE_LIB)

RES_DIR = _results/$(compiler)_$(interface)_$(threading)_$(_IA)_$(RES_EXT)$Z

libintel64 libem64t:
	$(MAKE) $(RES) EXT=a _IA=intel64 PLATF=intel64 RES_EXT=lib
dylibintel64 dylibem64t:
	$(MAKE) $(RES) EXT=dylib _IA=intel64 PLATF=intel64 RES_EXT=dylib

.PHONY: clean
clean:
	-rm -rf _results;

#-------------------------------------------------------------------------------

vpath %.f source

$(RES): %.res: %.f
	mkdir -p ./$(RES_DIR)
	$(FC) $(FOPTS) $< $(MKL_LIBS) $(LL_THREAD_LIB) $(RPATH) -lpthread -ldl -o $(RES_DIR)/$*.out
	@if [ -f data/$*.d ]; then \
		$(RES_DIR)/$*.out <data/$*.d >$(RES_DIR)/$@ ; \
	else \
		$(RES_DIR)/$*.out >$(RES_DIR)/$@ ; \
	fi

#-------------------------------------------------------------------------------
