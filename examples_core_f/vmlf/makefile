#===============================================================================
# Copyright 2001-2020 Intel Corporation.
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
##      Intel(R) Math Kernel Library VML examples creation and run
##******************************************************************************

help:
	@echo "Usage: make {libintel64|dylibintel64} [function=name]"
	@echo "[compiler=compiler_name] [interface=interface_name] [threading=threading_name]"
	@echo "[parallel=parallel_name]"
	@echo "name           - function name. Please see vml.lst file"
	@echo "compiler_name  - can be pgi or intel. Default value is intel."
	@echo "                 Intel(R) Fortran Compiler as default."
	@echo "                 If compiler=pgi then PGI Fortran 95 compiler will be used. Available only for intel64."
	@echo "interface_name - can be lp64 or ilp64 for intel64. Default value is lp64."
	@echo "threading_name - can be parallel or sequential. Default value is parallel."
	@echo "parallel_name  - can be intel or pgi (only if compiler=pgi). Default value is intel."

##------------------------------------------------------------------------------
## examples of using:
##
## make libintel64 function=vssin
##     build by Intel(R) Fortran compiler (as default)
##     and run VSSIN example for 64-bit applications, static linking
##
## make dylibintel64
##     build by Intel(R) Fortran compiler (as default) and run all examples of MKL
##     for 64-bit applications, dynamic linking
##------------------------------------------------------------------------------

include vml.lst

ifndef function
function = $(VML)
endif

ifeq (,$(filter gnu pgi,$(compiler)))
   override compiler=intel
   override parallel=intel
endif

ifneq ($(interface),ilp64)
   override interface=lp64
endif

ifneq ($(threading),sequential)
   override threading=parallel
endif

ifeq (,$(filter gnu pgi,$(parallel)))
   override parallel=intel
   override omp=iomp5
else
   override omp=pgmp
endif

RES = $(addsuffix .res ,$(function))

ifndef MKLROOT
   MKLROOT = ../..
endif
MKL_PATH = "$(MKLROOT)/lib"
CMPLR_PATH = "$(MKLROOT)/../compiler/lib"

ifeq ($(interface),ilp64)
   IFACE_LIB=$(MKL_PATH)/libmkl_$(IFACE_COMP_PART)_ilp64.$(EXT)
   FOPTS = -i8
else
   IFACE_LIB=$(MKL_PATH)/libmkl_$(IFACE_COMP_PART)_lp64.$(EXT)
   FOPTS =
endif

ifeq ($(compiler),intel)
   FC=ifort
   FOPTS += -w95 -fpp1 -warn errors 
endif

ifeq ($(compiler),pgi)
   FC=pgf95
   FOPTS += -Minform=severe -Mnokeepobj
endif

IFACE_COMP_PART=intel

ifeq ($(parallel),intel)
   IFACE_THREADING_PART=intel
endif

ifeq ($(parallel),pgi)
   IFACE_THREADING_PART=pgi
endif

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

RPATH = -Wl,-rpath,$(MKL_PATH) -Wl,-rpath,$(CMPLR_PATH)
CORE_LIB=$(MKL_PATH)/libmkl_core.$(EXT)

MKL_LIBS=$(IFACE_LIB) $(THREADING_LIB) $(CORE_LIB) $(OMP_LIB)

ifeq ($(threading),parallel)
  RES_DIR=_results/$(compiler)_$(interface)_$(threading)_$(parallel)_$(omp)_$(_IA)_$(RES_EXT)$Z
else
  RES_DIR=_results/$(compiler)_$(interface)_$(threading)_$(_IA)_$(RES_EXT)$Z
endif

libintel64 libem64t:
	$(MAKE) $(RES) EXT=a _IA=intel64 RES_EXT=lib
dylibintel64 dylibem64t:
	$(MAKE) $(RES) EXT=dylib _IA=intel64 RES_EXT=dylib

.PHONY: clean
clean:
	-rm -rf _results;

#-------------------------------------------------------------------------------

%.res: ./source/%.f
	mkdir -p ./$(RES_DIR)
	$(FC) $(FOPTS) -module ./$(RES_DIR) -I$(MKLROOT)/include $< $(MKL_LIBS) $(RPATH) -lm -lpthread -o $(RES_DIR)/$*.out
	$(RES_DIR)/$*.out >$(RES_DIR)/$@

#-------------------------------------------------------------------------------
