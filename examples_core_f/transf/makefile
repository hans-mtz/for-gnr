#===============================================================================
# Copyright 2010-2020 Intel Corporation.
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

## Content:
##     Intel(R) Math Kernel Library TRANS examples creation and run
##*****************************************************************************

help:
	@echo "To get help just run make or:"
	@echo "  make help"
	@echo
	@echo "To run TRANSF examples:"
	@echo "  make {libintel64|dylibintel64}"
	@echo "       [compiler=compiler_name]"
	@echo "       [interface=interface_name]"
	@echo "       [threading=threading_name]"
	@echo "       [parallel=parallel_name]"
	@echo "       [omp=omp_name]"
	@echo "       [function=name]"
	@echo
	@echo "Where:"
	@echo "  lib% targets are for static and dylib% for dynamic linkage"
	@echo
	@echo "  compiler_name  - {intel|pgi}"
	@echo "                   intel: Intel(R) Fortran Compiler"
	@echo "                   pgi:   Portland Group's Fortran 95 Compiler. Available only for intel64."
	@echo "                   Default value: intel"
	@echo
	@echo "  interface_name - {lp64|ilp64} (only for Intel(R) 64)"
	@echo "                   Default value: lp64"
	@echo
	@echo "  threading_name - {parallel|sequential}"
	@echo "                   Default value: parallel"
	@echo
	@echo "  parallel_name  - intel (default value)"
	@echo "                   pgi   (only if compiler=pgi)"
	@echo
	@echo "  omp_name       - iomp5 (if parallel=intel)"
	@echo "                   pgmp  (if parallel=pgi)"
	@echo "                   Default value depends on <parallel>"
	@echo
	@echo "  name           - function name"
	@echo "                   Please see trans.lst file"
	@echo
	@echo "Additional macros:"
	@echo "  MKLROOT=<path>"
	@echo "     Path to Intel(R) MKL root directory with header files and libraries."
	@echo "                   Default value: ../.."
	@echo
##------------------------------------------------------------------------------
## examples of using:
##
## make libintel64 function=simatcopy
##     build by Intel(R) C Compiler (as default)
##     and run SIMATCOPY example for 64-bit applications, static linking
##------------------------------------------------------------------------------
include trans.lst

ifndef function
   function = $(TRANS)
endif

ifneq ($(compiler),pgi)
   override compiler=intel
   override parallel=intel
endif

ifneq ($(interface),ilp64)
   override interface=lp64
endif

ifneq ($(threading),sequential)
   override threading=parallel
endif

ifneq ($(parallel),pgi)
   override parallel=intel
   override omp=iomp5
else
   override omp=pgmp
endif

ifndef MKLROOT
   MKLROOT = ../..
endif

RES = $(addsuffix .res ,$(function))
MKL_PATH = "$(MKLROOT)/lib/"
CMPLR_PATH = "$(MKLROOT)/../compiler/lib/"

ifeq ($(interface),ilp64)
   IFACE_LIB=$(MKL_PATH)/libmkl_$(IFACE_COMP_PART)_ilp64.$(EXT)
   FOPTS = -i8
else
   IFACE_LIB=$(MKL_PATH)/libmkl_$(IFACE_COMP_PART)_lp64.$(EXT)
   FOPTS =
endif

ifeq ($(compiler),pgi)
   FC=pgf95
   FOPTS.DIAG = 
else
   override FC=ifort
   FOPTS.DIAG = -warn all -warn errors
endif
IFACE_COMP_PART=intel

ifeq ($(compiler),pgi)
ifeq ($(threading),parallel)
ifeq ($(parallel),pgi)
ifeq (dylib,$(findstring dylib,$(MAKECMDGOALS)))
$(warning ***)
$(warning *** Target dylibintel64 is not available for pgi-parallel examples)
$(warning ***)
$(error Try >make help)
endif
endif
endif
endif

ifeq ($(parallel),pgi)
   IFACE_THREADING_PART=pgi
else
   IFACE_THREADING_PART=intel
endif

ifeq ($(threading),sequential)
   THREADING_LIB=$(MKL_PATH)/libmkl_sequential.$(EXT)
   OMP_LIB =
else
   THREADING_LIB=$(MKL_PATH)/libmkl_$(IFACE_THREADING_PART)_thread.$(EXT)
   ifeq ($(parallel),pgi)
      FOPTS += -mp
   endif
   ifeq ($(omp),pgmp)
      OMP_LIB =
   else
      OMP_LIB = -L$(CMPLR_PATH) -l$(omp)
   endif
endif

FOPTS += $(FOPTS.DIAG)
RPATH = -Wl,-rpath,$(MKL_PATH) -Wl,-rpath,$(CMPLR_PATH)
CORE_LIB=$(MKL_PATH)/libmkl_core.$(EXT)

MKL_LIBS=$(IFACE_LIB) $(THREADING_LIB) $(CORE_LIB) $(OMP_LIB)

ifeq ($(threading),parallel)
  RES_DIR=_results/$(compiler)_$(interface)_$(threading)_$(parallel)_$(omp)_$(_IA)_$(RES_EXT)
else
  RES_DIR=_results/$(compiler)_$(interface)_$(threading)_$(_IA)_$(RES_EXT)
endif

libintel64  libem64t:
	$(MAKE) $(RES) EXT=a _IA=intel64 RES_EXT=lib
dylibintel64 dylibem64t:
	$(MAKE) $(RES) EXT=dylib _IA=intel64 RES_EXT=dylib

.PHONY: clean
clean:
	-rm -rf _results;

#-------------------------------------------------------------------------------

vpath %.f90 source

$(RES): %.res: %.f90
	mkdir -p ./$(RES_DIR)
	$(FC) $(FOPTS) $(RPATH) $< source/common_func.f -I"$(MKLROOT)/include" $(MKL_LIBS) $(RPATH) -lpthread -o $(RES_DIR)/$*.out
	$(RES_DIR)/$*.out >$(RES_DIR)/$@

#-------------------------------------------------------------------------------
