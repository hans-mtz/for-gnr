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
##      Intel(R) Math Kernel Library Trigonometric Transforms examples creation
##      and run
##
##******************************************************************************


help:
	@echo "Usage: make {libintel64|dylibintel64} [function=name]"
	@echo "[interface=interface_name] [threading=threading_name]"
	@echo
	@echo "name           - function name. Please see pdettf.lst file"
	@echo "                 Intel(R) Fortran Compiler as default"
	@echo "interface_name - can be lp64 or ilp64 for intel64. Default value is lp64."
	@echo
	@echo "threading_name - can be parallel or sequential. Default value is parallel."

##------------------------------------------------------------------------------
## example of usage:
##
## make libintel64 function=d_trig_tforms_bvp
##     build by Intel(R) Fortran Compiler (as default) and
##     run d_trig_tforms_bvp example for 64-bit applications, static linking
##------------------------------------------------------------------------------

include pdettf.lst

ifndef function
function = $(PDE_TT_F)
endif

ifndef compiler
compiler=intel
endif

ifndef interface
interface=lp64
endif

ifndef threading
threading=parallel
endif

RES = $(addsuffix .res ,$(function))
MOD = $(addsuffix .mod ,$(modules))

ifndef MKLROOT
MKLROOT = ../..
endif
MKL_PATH = "$(MKLROOT)/lib"
CMPLR_PATH = "$(MKLROOT)/../compiler/lib"

override FC=ifort -warn all -warn errors
IFACE_COMP_PART=intel
IFACE_THREADING_PART=intel

ifeq ($(interface),ilp64)
IFACE_LIB=$(MKL_PATH)/libmkl_$(IFACE_COMP_PART)_ilp64.$(EXT)
FOPTS = -i8
else
IFACE_LIB=$(MKL_PATH)/libmkl_$(IFACE_COMP_PART)_lp64.$(EXT)
FOPTS =
endif

ifeq ($(threading),sequential)
THREADING_LIB=$(MKL_PATH)/libmkl_sequential.$(EXT)
OMP_LIB =
else
THREADING_LIB=$(MKL_PATH)/libmkl_$(IFACE_THREADING_PART)_thread.$(EXT)
OMP_LIB = -L$(CMPLR_PATH) -liomp5
endif

RPATH = -Wl,-rpath,$(MKL_PATH) -Wl,-rpath,$(CMPLR_PATH)
CORE_LIB=$(MKL_PATH)/libmkl_core.$(EXT)

FOPTS += -I$(MKLROOT)/include

MKL_LIBS=$(IFACE_LIB) $(THREADING_LIB) $(CORE_LIB) $(OMP_LIB)

RES_DIR=_results/$(compiler)_$(interface)_$(threading)_$(_IA)_$(RES_EXT)$Z

libintel64 libem64t: clean_files
	$(MAKE) $(MOD) $(RES) _IA=intel64 EXT=a RES_EXT=lib
dylibintel64 dylibem64t: clean_files
	$(MAKE) $(MOD) $(RES) _IA=intel64 EXT=dylib RES_EXT=dylib
clean_files:
	rm -rf *.mod *.o;

.PHONY: clean
clean:
	-rm -rf _results;
#-------------------------------------------------------------------------------

vpath %.f90 source
vpath %.f90 $(MKLROOT)/include

.NOTPARALLEL: # as written, this can only be run with make -j1, to support creation of .mod files first

$(MOD): %.mod: %.f90
	mkdir -p ./$(RES_DIR)
	$(FC) $(FOPTS) -c $<

$(RES): %.res: %.f90
	mkdir -p ./$(RES_DIR)
	$(FC) $(FOPTS) $< -L$(MKL_PATH) $(MKL_LIBS) $(RPATH) \
			-lpthread -o $(RES_DIR)/$*.out
	$(RES_DIR)/$*.out >$(RES_DIR)/$@

#-------------------------------------------------------------------------------
