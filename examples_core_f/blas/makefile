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
##      Intel(R) Math Kernel Library BLAS examples creation and run
##******************************************************************************

help:
	@echo "Usage: make {libintel64|dylibintel64} [function=name]"
	@echo "[compiler=compiler_name] [interface=interface_name] [threading=threading_name]"
	@echo "[parallel=parallel_name] [omp=omp_name]"
	@echo "name           - function name. Please see blas.lst file."
	@echo "compiler_name  - can be pgi or intel. Default value is intel."
	@echo "                 If compiler=pgi then PGI Fortran 95 compiler will be used. Available only for intel64."
	@echo "                 Intel (R) Fortran Compiler as default."
	@echo "interface_name - can be lp64 or ilp64 for intel64. Default value is lp64."
	@echo "threading_name - can be parallel or sequential. Default value is parallel."
	@echo "parallel_name  - can be intel or pgi (only if compiler=pgi). Default value is intel."
	@echo "omp_name       - can be iomp5 if parallel=intel or can be pgmp if parallel=pgi."
	@echo "                 Default value is iomp5."
	@echo "Note: In this Intel(R) MKL version the DYNAMIC linking on Mac OS"
	@echo "      (i.e. target dylibintel64)"
	@echo "      is not available for such a combination of parameters:"
	@echo "      compiler=pgi threading=parallel parallel=pgi"

##------------------------------------------------------------------------------
## example of using:
##
## make libintel64 function=dgemm
##     build by Intel(R) Fortran Compiler (as default) and
##     run DGEMM  example  for  64-bit  applications, static linking
##------------------------------------------------------------------------------

include blas.lst

ifndef function
   function = $(BLAS) $(BLAS_90)
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

RES = $(addsuffix x.res ,$(function))
RES_90 = $(addsuffix x.res ,$(filter $(BLAS_90), $(function)))
RES_77 = $(addsuffix x.res ,$(filter-out $(BLAS_90), $(function)))

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
      FOPTS=-fdefault-integer-8
   endif
else
   IFACE_LIB=$(MKL_PATH)/libmkl_$(IFACE_COMP_PART)_lp64.$(EXT)
endif

ifeq ($(compiler),gnu)
   FC=gfortran
   ifeq ($(FC),gfortran)
      IFACE_COMP_PART=gf
   else
      IFACE_COMP_PART=intel
   endif
   FOPTS += -Werror
   MODDIROPT = -J
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

FOPTS += -I$(MKLROOT)/include

ifeq ($(threading),parallel)
  RES_DIR=_results/$(compiler)_$(interface)_$(threading)_$(parallel)_$(omp)_$(_IA)_$(RES_EXT)$Z
else
  RES_DIR=_results/$(compiler)_$(interface)_$(threading)_$(_IA)_$(RES_EXT)$Z
endif

libintel64  libem64t:
	$(MAKE) $(RES) EXT=a _IA=intel64 RES_EXT=lib
dylibintel64 dylibem64t:
	$(MAKE) $(RES) EXT=dylib _IA=intel64 RES_EXT=dylib

.PHONY: clean
clean:
	-rm -rf _results;

#-------------------------------------------------------------------------------

vpath %.f source
vpath %.f90 source

$(RES_77): %.res: %.f
	mkdir -p ./$(RES_DIR)/$*_modules
	$(FC) $(FOPTS) $< source/common_func.f $(MKL_LIBS) $(RPATH) \
	-lpthread -o $(RES_DIR)/$*.out $(MODDIROPT) $(RES_DIR)/$*_modules
	$(RES_DIR)/$*.out <data/$*.d >$(RES_DIR)/$@
	rm -rf ./$(RES_DIR)/$*_modules

$(RES_90): %.res: %.f90
	mkdir -p ./$(RES_DIR)/$*_modules
	$(FC) $(FOPTS) $< source/common_func.f $(MKL_LIBS) $(RPATH) \
	-lpthread -o $(RES_DIR)/$*.out $(MODDIROPT) $(RES_DIR)/$*_modules
	$(RES_DIR)/$*.out <data/$*.d >$(RES_DIR)/$@
	rm -rf ./$(RES_DIR)/$*_modules

#-------------------------------------------------------------------------------
