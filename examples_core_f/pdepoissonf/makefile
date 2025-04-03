#===============================================================================
# Copyright 2006-2020 Intel Corporation.
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
##      Intel(R) Math Kernel Library Poisson Solver examples creation
##      and run
##
##******************************************************************************


help:
	@echo "Usage: make {libintel64|dylibintel64} [function=name]"
	@echo "[interface=interface_name] [threading=threading_name]"
	@echo
	@echo "name           - function name. Please see pdepoissonf.lst file"
	@echo
	@echo "interface_name - can be lp64 or ilp64 for intel64. Default value is lp64."
	@echo
	@echo "threading_name - can be omp, tbb or sequential. Default value is omp."

##------------------------------------------------------------------------------
## example of usage:
##
## make libintel64 function=d_Helmholtz_2D_f
##     build by Intel(R) Fortran Compiler (as default) and
##     run d_Helmholtz_2D_f example for 64-bit applications, static linking
##------------------------------------------------------------------------------

include pdepoissonf.lst

function = $(PDE_POISSON_F)
override compiler=intel
interface=lp64
threading=omp

RES = $(addsuffix .res ,$(function))
MOD = $(addsuffix .mod ,$(modules))

ifndef MKLROOT
MKLROOT = ../..
endif
MKL_PATH = "$(MKLROOT)/lib"
CMPLR_PATH = "$(MKLROOT)/../compiler/lib"
TBB_PATH = "$(MKLROOT)/../tbb/lib"

override FC=ifort -w
IFACE_COMP_PART=intel
IFACE_THREADING_PART=intel

ifeq ($(interface),ilp64)
IFACE_LIB=$(MKL_PATH)/libmkl_$(IFACE_COMP_PART)_ilp64.$(EXT)
FOPTS = -i8
else
IFACE_LIB=$(MKL_PATH)/libmkl_$(IFACE_COMP_PART)_lp64.$(EXT)
FOPTS =
endif

ifeq ($(threading),tbb)
THREADING_LIB=$(MKL_PATH)/libmkl_tbb_thread.$(EXT)
LL_THREAD_LIB.a     = -L$(TBB_PATH) -ltbb -lc++
LL_THREAD_LIB.dylib =
LL_THREAD_LIB      := $(LL_THREAD_LIB.$(EXT))
else
ifeq ($(threading),sequential)
THREADING_LIB=$(MKL_PATH)/libmkl_sequential.$(EXT)
LL_THREAD_LIB =
else
THREADING_LIB=$(MKL_PATH)/libmkl_$(IFACE_THREADING_PART)_thread.$(EXT)
LL_THREAD_LIB = -L$(CMPLR_PATH) -liomp5
endif
endif

RPATH = -Wl,-rpath,$(MKL_PATH) -Wl,-rpath,$(CMPLR_PATH) -Wl,-rpath,$(TBB_PATH)
CORE_LIB=$(MKL_PATH)/libmkl_core.$(EXT)

MKL_LIBS=$(IFACE_LIB) $(THREADING_LIB) $(CORE_LIB) $(LL_THREAD_LIB)

RES_DIR=_results/$(compiler)_$(interface)_$(threading)_$(_IA)_$(RES_EXT)$Z

libintel64 libem64t: clean_files
	$(MAKE) $(MOD) $(RES) _IA=intel64 EXT=a RES_EXT=lib
dylibintel64 dylibem64t: clean_files
	$(MAKE) $(MOD) $(RES) _IA=intel64 EXT=dylib RES_EXT=dylib
clean_files:
	rm -rf *.mod *.o _results;

.PHONY: clean
clean:
	-rm -rf _results;

#-------------------------------------------------------------------------------

vpath %.f90 source
vpath %.f90 $(MKLROOT)/include

.NOTPARALLEL: # as written, this can only be run with make -j1, to support creation of .mod files first

$(MOD): %.mod: %.f90
	mkdir -p ./$(RES_DIR)
	$(FC) $(FOPTS) -w -c $<

$(RES): %.res: %.f90
	mkdir -p ./$(RES_DIR)
	$(FC) $(FOPTS) -w $< -L$(MKL_PATH) $(MKL_LIBS) $(RPATH) \
			-lpthread -o $(RES_DIR)/$*.out
	$(RES_DIR)/$*.out >$(RES_DIR)/$@
#-------------------------------------------------------------------------------
