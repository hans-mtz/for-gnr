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
##      Intel(R) Math Kernel Library BLAS95 examples creation and run
##******************************************************************************

help:
	@echo "Usage: make {libintel64|dylibintel64} [function=name]"
	@echo "[compiler=compiler_name] [interface=interface_name] [threading=threading_name]"
	@echo "[parallel=parallel_name] [omp=omp_name]"
	@echo "[F95ROOT=<f95_root_path>] [MKLROOT=<MKL_root_path>]"
	@echo "name           - function name. Please see blas95.lst file."
	@echo "compiler_name  - can be pgi or intel. Default value is intel."
	@echo "                 If compiler=pgi then PGI Fortran 95 compiler will be used. Available only for intel64."
	@echo "                 Intel (R) Fortran Compiler as default."
	@echo "interface_name - can be lp64 or ilp64 for intel64. Default value is lp64."
	@echo "threading_name - can be parallel or sequential. Default value is parallel."
	@echo "parallel_name  - can be intel or pgi (only if compiler=pgi). Default value is intel."
	@echo "omp_name       - can be iomp5 if parallel=intel or can be pgmp if parallel=pgi."
	@echo "                 Default value is iomp5."
	@echo "f95_root_path  - Path to blas95 library and .mod files that are built on user side."
	@echo "                 Mandatory if compiler is not intel. To build library and .mod files,"
	@echo "                 go to the directory <MKLROOT>/interfaces/blas95 and run:"
	@echo "                 make <target> [options] FC=<f95compiler> INSTALL_DIR=<f95_root_path>"
	@echo "                 See description of the FC and INSTALL_DIR in the interface makefile."
	@echo "MKL_root_path  - Locate Intel(R) MKL libraries relative to <MKL_root_path>."
	@echo "                 Default value: ../.., unless defined in environment."
	@echo "Note: In this Intel(R) MKL version the DYNAMIC linking on Mac OS"
	@echo "      (i.e. target dylibintel64)"
	@echo "      is not available for such a combination of parameters:"
	@echo "      compiler=pgi threading=parallel parallel=pgi"

##------------------------------------------------------------------------------
## example of using:
##
## make libintel64 function=dgemmx
##     build by Intel(R) Fortran Compiler (as default) and
##     run DGEMM  example  for  64-bit  applications, static linking
##------------------------------------------------------------------------------

include blas95.lst

ifndef function
   function = $(SRC_F90)
endif

ifeq (,$(filter pgi,$(compiler)))
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
#   ifeq ($(parallel),gnu)
#      ifneq ($(omp),gomp)
#      override omp=iomp5
#      endif
#   else
      override omp=pgmp
#   endif
endif

RES = $(addsuffix .res ,$(function))

ifndef MKLROOT
MKLROOT = ../..
endif
MKL_PATH = "$(MKLROOT)/lib"
CMPLR_PATH = "$(MKLROOT)/../compiler/lib"

FOPTS     =
LOPTS     =
MODDIROPT = -module

ifeq ($(interface),ilp64)
   IFACE_LIB=$(MKL_PATH)/libmkl_$(IFACE_COMP_PART)_ilp64.$(EXT)
   FOPTS += -i8
   ifeq ($(compiler),gnu)
      override FC=gfortran
      FOPTS += -fdefault-integer-8
   endif
else
   IFACE_LIB=$(MKL_PATH)/libmkl_$(IFACE_COMP_PART)_lp64.$(EXT)
endif

ifeq ($(compiler),gnu)
   FC=gfortran
   ifeq ($(FC),gfortran)
      IFACE_COMP_PART=gf
      FOPTS += -Werror
      MODDIROPT = -J
   else
      IFACE_COMP_PART=intel
   endif
else
   ifeq ($(compiler),pgi)
      FC=pgf95
   else
      override FC=ifort
      FOPTS += -warn errors -nogen-interfaces
   endif
   IFACE_COMP_PART=intel
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

#ifeq ($(parallel),gnu)
#   IFACE_THREADING_PART=gnu
#   GFORTRAN_LIB=-L$(gnu_path) -lgfortran
#else
      GFORTRAN_LIB=
   ifeq ($(parallel),pgi)
      IFACE_THREADING_PART=pgi
   else
      IFACE_THREADING_PART=intel
   endif
#endif

ifeq ($(threading),sequential)
   THREADING_LIB=$(MKL_PATH)/libmkl_sequential.$(EXT)
   OMP_LIB =
   GFORTRAN_LIB=
else
   THREADING_LIB=$(MKL_PATH)/libmkl_$(IFACE_THREADING_PART)_thread.$(EXT)
   ifeq ($(parallel),pgi)
      FOPTS += -mp
   endif
#   ifeq ($(omp),gomp)
#      OMP_LIB = -L$(gnu_path) -l$(omp)
#   else
      ifeq ($(omp),pgmp)
        OMP_LIB =
      else
        OMP_LIB = -L$(CMPLR_PATH) -l$(omp)
      endif
#   endif
endif

RPATH=-Wl,-rpath,$(MKL_PATH) -Wl,-rpath,$(CMPLR_PATH)
CORE_LIB=$(MKL_PATH)/libmkl_core.$(EXT)

MKL_LIBS=$(IFACE_LIB) $(THREADING_LIB) $(CORE_LIB) $(OMP_LIB)

ifeq ($(threading),parallel)
  RES_DIR=_results/$(compiler)_$(interface)_$(threading)_$(parallel)_$(omp)_$(_IA)_$(RES_EXT)$Z
else
  RES_DIR=_results/$(compiler)_$(interface)_$(threading)_$(_IA)_$(RES_EXT)$Z
endif

ifndef F95ROOT
   F95ROOT = $(MKLROOT)
   ifneq ($(compiler),intel)
     $(error F95ROOT is mandatory parameter for $(compiler))
   endif
endif

MODPATH=$(F95ROOT)/include/$(_IA)/$(interface)
LIB95=$(F95ROOT)/lib/libmkl_blas95_$(interface).a

libintel64 libem64t:
	$(MAKE) $(RES) EXT=a _IA=intel64 RES_EXT=lib
dylibintel64 dylibem64t:
	$(MAKE) $(RES) EXT=dylib _IA=intel64 RES_EXT=dylib

.PHONY: clean
clean:
	-rm -rf _results;

#-------------------------------------------------------------------------------

vpath %.f90 source

$(RES): %.res: %.f90
	mkdir -p ./$(RES_DIR)/$*_modules
	export DYLD_LIBRARY_PATH=$(DYLD_LIBRARY_PATH):"$(gnu_path)"; $(FC) $(FOPTS) -I $(MODPATH) $< source/common_func.f $(LIB95) $(MKL_LIBS) $(RPATH) -lpthread -o $(RES_DIR)/$*.out $(MODDIROPT) $(RES_DIR)/$*_modules
	rm -rf ./$(RES_DIR)/$*_modules
	$(RES_DIR)/$*.out <data/$*.d >$(RES_DIR)/$@

#-------------------------------------------------------------------------------
