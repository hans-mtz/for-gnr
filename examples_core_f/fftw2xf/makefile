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

## Content:
##      Build and run examples using FFTW2 Fortran wrappers to Intel MKL.
##*****************************************************************************

help:
	@echo "To run FFTW2F examples:"
	@echo "  make {libintel64|dylibintel64}"
	@echo "       [precision={MKL_DOUBLE|MKL_SINGLE}]"
	@echo "       [compiler=<name>] [threading=<name>]"
	@echo "       [parallel=<name>] [omp=<name>] [function=<name>]"
	@echo
	@echo "To get report on run examples:"
	@echo "  make report"
	@echo
	@echo "To clean results:"
	@echo "  make clean"
	@echo
	@echo "To get help just run make or:"
	@echo "  make help"
	@echo
	@echo "Main options:"
	@echo "  targets lib%   use static linkage"
	@echo "  targets dylib% use dynamic linkage"
	@echo
	@echo "  precision=<tag> selects type of floating-point precision:"
	@echo "      precision=MKL_DOUBLE - double precision (DEFAULT)"
	@echo "      precision=MKL_SINGLE - single precision"
	@echo
	@echo "  compiler=<name> selects the compiler to build the examples:"
	@echo "      compiler=pgi   - PGI pgf95. Available only for intel64."
	@echo "      compiler=intel - Intel(R) Fortran compiler ifort (DEFAULT)"
	@echo
	@echo "  threading=<name> selects threading of MKL:"
	@echo "      threading=parallel   - multithreaded version (DEFAULT)"
	@echo "      threading=sequential - sequential version"
	@echo
	@echo "  parallel=<name> selects Intel(R) MKL threading layer for threading=parallel:"
	@echo "      parallel=intel - libmkl_intel_thread"
	@echo "      parallel=pgi   - libmkl_pgi_thread"
	@echo "      Default value depends on the setting of compiler=<name>"
	@echo
	@echo "  omp=<name> selects OpenMP runtime library for threading=parallel:"
	@echo "      omp=iomp5 - Intel OpenMP runtime"
	@echo "      omp=pgmp  - PGI OpenMP runtime"
	@echo "      Default value depends on the setting of parallel=<name>"
	@echo
	@echo "  function=<name> selects examples to execute"
	@echo "      Default value: all examples listed in file fftw2xf.lst"
	@echo
	@echo "Additional options:"
	@echo "  RES_DIR=<path> defines where to place the results"
	@echo "      Default value: ./_results"
	@echo
	@echo "  MKLROOT=<path> defines alternative Intel(R) MKL root directory"
	@echo "      Default value: ../.."
	@echo
	@echo "  INSTALL_DIR=<path> defines path to fftw2 wrapper library."
	@echo '      Default value: ./wrap_lib_$$(compiler)'
	@echo
	@echo "  MKLRUNLIB_PATH=<path> defines alternative runtime library directory"
	@echo "      for dynamic linkage."
	@echo '      Default value: $$(MKLROOT)/lib/$$(IA), with $$(IA) defined by the target'
	@echo
	@echo "  LIB_PATH=<path> defines location of OpenMP runtime libraries"
	@echo "      needed by GNU or PGI threading layer. Set this value if"
	@echo "      respective libraries (libgomp, libpgmp) cannot be found in"
	@echo "      LIBRARY_PATH and DYLD_LIBRARY_PATH"
	@echo
	@echo "  TARGET_ARCH=<flags> defines additional compiler flags"
	@echo "      Refer to the compiler documentation about the architecture specific"
	@echo "      flags. For example, Intel compilers may need TARGET_ARCH=-xAVX"
	@echo "      to generate Intel(R) AVX instructions."

##-----------------------------------------------------------------------------
## Usage examples:
##
## make dylibintel64 precision=MKL_SINGLE
##     Use Intel(R) Fortran compiler to build single-precision wrapper library,
##     build and run examples linked dynamically to MKL
##     as applications for Intel(R) 64 processor family
##-----------------------------------------------------------------------------
## Default values

MY_MAKEFILE := $(MAKEFILE_LIST)

ifndef MKLROOT
  MKLROOT = ../..
endif

ifndef INSTALL_DIR
  INSTALL_DIR = .
endif

ifndef MKLRUNLIB_PATH
  MKLRUNLIB_PATH = $(MKLROOT)/lib
endif

ifndef RES_DIR
  RES_DIR = _results
endif

include fftw2xf.lst

ifneq ($(precision),MKL_SINGLE)
  override precision = MKL_DOUBLE
  prec = double
else
  prec = single
endif

ifndef function
  function = $(DFT_$(precision))
endif

COMMON = mkl_fftw_examples_support.f

RES = $(addsuffix .res,$(function))

ifeq (,$(filter gnu pgi,$(compiler)))
  override compiler = intel
  override parallel = intel
endif

# FFTW2 wrappers support lp64 only
ifeq (intel64,$(_IA))
  override interface = lp64
  _lp64 = _$(interface)
else
  override interface =
endif

ifeq (,$(filter parallel sequential,$(threading)))
  override threading = parallel
endif

ifneq ($(threading),sequential)
_parallel_intel = intel
_parallel_gnu = intel
_parallel_pgi = pgi

_omp_intel = iomp5
_omp_gnu = iomp5
_omp_pgi = pgmp

parallel = $(_parallel_$(compiler))
omp = $(_omp_$(parallel))
endif

ifeq (,$(filter gomp pgmp,$(omp)))
  override omp = iomp5
endif

ifeq (,$(filter gnu pgi,$(parallel)))
  override parallel = intel
  override omp = iomp5
else
  ifeq ($(compiler)-$(parallel),gnu-gnu)
    ifneq ($(omp),gomp)
      override omp = iomp5
    endif
  else
    ifeq ($(compiler)-$(parallel),pgi-pgi)
      override omp = pgmp
    else
      override omp = iomp5
    endif
  endif
endif

ifeq ($(compiler),pgi)
ifeq ($(threading),parallel)
ifeq ($(parallel),pgi)
ifeq ($(_LIB),dylib)
ifeq ($(MKL_LIBS_LINK),mixed)
	override MKL_LIBS_LINK =
endif
endif
endif
endif
endif

ifeq ($(compiler),gnu)
  FC = gfortran
  FOPTS.intel64 = -m64
  FOPTS.DIAG = -Wall -Werror
  FOPTS.MOD =
  IFACE_COMP_PART = intel# gf missing
else
  ifeq ($(compiler),pgi)
    FC = pgf95
  else
    FC = ifort
    FOPTS.DIAG = -warn all -warn errors
  endif
  FOPTS.MOD = -module $(res_dir)
  IFACE_COMP_PART = intel
endif

ifeq ($(compiler)-$(parallel),gnu-gnu)
  IFACE_THREADING_PART = intel# gnu missing
else
  ifeq ($(compiler)-$(parallel),pgi-pgi)
    IFACE_THREADING_PART = pgi
  else
    IFACE_THREADING_PART = intel
  endif
endif

ifeq ($(FC),gfortran)
  FCOPTS = -w -D$(precision) -I$(MKLROOT)/include -I$(MKLROOT)/include/fftw \
           -Isource -fsecond-underscore
else
  FCOPTS = -w -D$(precision) -I$(MKLROOT)/include -I$(MKLROOT)/include/fftw
endif

ifeq ($(interface),ilp64)
  # Currently FFTW2 FORTRAN wrappers are not ready for INTEGER*8
  NA.WRAPLIB_MAKEFLAGS.intel64 = interface=ilp64
  ifeq ($(compiler),gnu)
    NA.FOPTS.intel64 += -fdefault-integer-8
  else
    NA.FOPTS.intel64 += -i8
  endif
  IFACE_LIB.intel64 = mkl_$(IFACE_COMP_PART)_ilp64
else
  IFACE_LIB.intel64 = mkl_$(IFACE_COMP_PART)_lp64
endif
IFACE_LIB = $(IFACE_LIB.$(_IA))

WRAPLIB_MAKEFLAGS = $(WRAPLIB_MAKEFLAGS.$(_IA))

FOPTS = $(FOPTS.$(_IA)) $(FOPTS.DIAG) $(FOPTS.MOD)

ifeq ($(threading),sequential)
  threadname = $(threading)
  threadlayer = $(threading)
  THREADING_LIB = mkl_sequential
  OMP_LIB =
else
  threadname = $(threading)_$(omp)
  threadlayer = $(parallel)
  THREADING_LIB = mkl_$(IFACE_THREADING_PART)_thread
  ifeq ($(omp),iomp5)
    OMP_LIB = -L$(CMPLR_PATH) -l$(omp)
  else
    ifneq ($(LIB_PATH),)
      OMP_LIB = -L$(LIB_PATH)
    endif
    OMP_LIB += -l$(omp)
  endif
endif

CORE_LIB = mkl_core

WRAP_LIB_DIR = $(INSTALL_DIR)/wrap_lib_$(compiler)/$(_IA)
WRAP_LIB_NAME = fftw2xf_$(prec)$(_lp64)
WRAP_LIB_EXT = a

ifeq ($(compiler),pgi)
  FOPTS += -mp -Wl,-no_compact_unwind
endif

MKL_PATH = $(MKLROOT)/lib

CMPLR_PATH = $(MKLROOT)/../compiler/lib

# Finally compose the MKL_LIBS
ifeq ($(_LIB),dylib)
  ifneq ($(MKL_LIBS_LINK),mixed)
    MKL_LIBS = -L$(MKL_PATH) -lmkl_rt
  else
    MKL_LIBS = -L$(MKL_PATH) -l$(IFACE_LIB) -l$(THREADING_LIB) -l$(CORE_LIB)
  endif
else
  MKL_LIBS += $(MKL_PATH)/lib$(IFACE_LIB).a
  MKL_LIBS += $(MKL_PATH)/lib$(THREADING_LIB).a
  MKL_LIBS += $(MKL_PATH)/lib$(CORE_LIB).a
endif

RUNENV = DYLD_BIND_AT_LAUNCH=1

ifneq ($(LIB_PATH),)
  RUNENV += DYLD_LIBRARY_PATH=$(LIB_PATH):$(DYLD_LIBRARY_PATH)
endif

ifeq ($(_LIB),dylib)
  RPATH = -Wl,-rpath,$(MKLRUNLIB_PATH) -Wl,-rpath,$(CMPLR_PATH) -Wl,-rpath,$(TBB_PATH)
  ifneq ($(MKL_LIBS_LINK),mixed)
    RUNENV += MKL_INTERFACE_LAYER=$(interface) MKL_THREADING_LAYER=$(threadlayer)
  endif
else
  RPATH = -Wl,-rpath,$(CMPLR_PATH)
endif

res_dir = $(RES_DIR)/$(compiler)$(_lp64)_$(threadname)_$(_LIB)$(_IA)_$(prec)

COMMON.obj = $(COMMON:%.f=$(res_dir)/%.o)

ifdef VERBOSE
  $(info )
  $(info MAKELEVEL=$(MAKELEVEL))
  $(info MKLROOT=$(MKLROOT) RES_DIR=$(RES_DIR))
  $(info compiler=$(compiler) interface=$(interface) threading=$(threading) \
         parallel=$(parallel) omp=$(omp))
endif

##-----------------------------------------------------------------------------
## Rules

vpath %.f source

.SUFFIXES:
.SUFFIXES: .f .res .out

.PHONY: libem64t libintel64 dylibem64t dylibintel64 \
	clean cleanup run mkresdir wrap_lib prepare_run FORCE

libintel64 libem64t:
	$(MAKE) -f $(MY_MAKEFILE) prepare_run _IA=intel64 _LIB=lib
	$(MAKE) -f $(MY_MAKEFILE) run _IA=intel64 _LIB=lib

dylibem64t dylibintel64:
	$(MAKE) -f $(MY_MAKEFILE) prepare_run _IA=intel64 _LIB=dylib
	$(MAKE) -f $(MY_MAKEFILE) run _IA=intel64 _LIB=dylib

prepare_run: mkresdir wrap_lib $(COMMON.obj)

mkresdir:
	@echo
	@echo See results in $(res_dir)
	@echo
	mkdir -p $(res_dir)

wrap_lib: $(WRAP_LIB_DIR)/lib$(WRAP_LIB_NAME).$(WRAP_LIB_EXT)

# We shall use absolute path for WRAP_LIB_DIR
_install_to=$(shell mkdir -p $(WRAP_LIB_DIR); cd $(WRAP_LIB_DIR) && pwd)
$(WRAP_LIB_DIR)/lib$(WRAP_LIB_NAME).$(WRAP_LIB_EXT):
	@echo
	# build $@
	cd $(MKLROOT)/interfaces/fftw2xf && $(MAKE) lib$(_IA) \
	   PRECISION=$(precision) compiler=$(compiler) \
	   INSTALL_DIR=$(_install_to) $(WRAPLIB_MAKEFLAGS) \
	   INSTALL_LIBNAME=lib$(WRAP_LIB_NAME).$(WRAP_LIB_EXT)

$(res_dir)/%.o: %.f
	# compile $*.f
	$(FC) $(FCOPTS) $(FOPTS) $(FFLAGS) $(TARGET_ARCH) \
	  -c $< -o $@

run: $(RES)

.PRECIOUS: $(res_dir)/%.out
$(res_dir)/%.out: %.f FORCE
	@echo
	# compile $*.f
	$(FC) $(FCOPTS) $(FOPTS) $(FFLAGS) $(TARGET_ARCH) \
	  $< \
	  $(COMMON.obj) \
	  -L$(WRAP_LIB_DIR) -l$(WRAP_LIB_NAME) \
	  $(MKL_LIBS) $(RPATH) \
	  $(OMP_LIB) -lpthread -lm -o $(res_dir)/$*.out

%.res: mkresdir $(res_dir)/%.out FORCE
	-rm -f $(res_dir)/$@ # remove $@ file
	@echo
	# run $*.out
	$(RUNENV) \
	  $(res_dir)/$*.out > $(res_dir)/$@

report: FORCE
	@echo
	@echo "--------------------------------------------------"
	@echo There are `ls source/*_ex[0-9]*.f | wc -l` \
	  examples in source
	@echo
	@for D in `ls -d $(RES_DIR)/* 2>/dev/null`; do \
	  echo -n "In   $$D     Total run examples:" \
	    "`ls $$D/*.res 2>/dev/null | wc -l`"; \
	  echo -n "     and Passed: "; \
	  cat /dev/null $$D/*.res 2>/dev/null | grep -c PASSED | cat; \
	done
	@echo "--------------------------------------------------"
	@echo

clean: FORCE
	-rm -rf $(RES_DIR) # clean all results

cleanup: clean
	-rm -rf $(INSTALL_DIR)/wrap_lib_* # clean all built wrap_libs

FORCE: ;

##-----------------------------------------------------------------------------
