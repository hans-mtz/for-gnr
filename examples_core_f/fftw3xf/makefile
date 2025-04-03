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
##      Build and run examples using FFTW3 Fortran wrappers to Intel MKL.
##*****************************************************************************

## Usage examples:
##
## make libintel64 function=complex_1d_double_ex1 threading=sequential
##        Build and run one example using Intel(R) MKL for Intel(R) 64
##        processor family applications with sequential layer.

help:
	@echo "Usage: make {libintel64|dylibintel64} [option...]"
	@echo
	@echo "Options:"
	@echo "  function=<name>"
	@echo "        Run only the specified example. Please see file"
	@echo "        fftw3xf.lst for the list of functions."
	@echo
	@echo "  compiler=pgi|intel"
	@echo "        Build examples using PGI pgf95 (only for intel64), or"
	@echo "        Intel(R) Fortran compiler ifort."
	@echo "        Default value: intel"
	@echo
	@echo "  INTERFACE=ilp64|lp64"
	@echo "        For Intel(R) 64 only, use ILP64 Intel(R) MKL interface layer"
	@echo "        when INTEGER is INTEGER*8"
	@echo "        Default value: lp64"
	@echo
	@echo "  threading=sequential|parallel"
	@echo "        Build examples using parallel or sequential MKL."
	@echo "        Default value: parallel"
	@echo
	@echo "  parallel=pgi|intel"
	@echo "        For threading=parallel, select Intel(R) MKL threading layer."
	@echo "        Default value depends on <compiler>"
	@echo
	@echo "  omp=pgmp|iomp5"
	@echo "        For threading=parallel, select system OpenMP library:"
	@echo "          iomp5 (if compiler=intel)"
	@echo "          pgmp  (if compiler=pgi)"
	@echo "        Default value depends on <parallel>"
	@echo
	@echo "  wraplib=yes|no"
	@echo "        Build and use standalone FFTW3 wrappers library."
	@echo "        The library will be built with respective C compiler"
	@echo '        and put into $$(INSTALL_DIR)/wrap_lib_$$(IA)'
	@echo "        Default value: no (that is, use integrated wrappers)"
	@echo
	@echo "Additional macros:"
	@echo "  RES_DIR=<path>"
	@echo "        Use <path> for building the examples."
	@echo "        Default value: ./_results"
	@echo
	@echo "  MKLROOT=<path>"
	@echo "        Path to Intel(R) MKL root directory with header files and libraries."
	@echo "        Default value: ../.."
	@echo
	@echo "  INSTALL_DIR=<path>"
	@echo "        Use <path> for installing of wraplib, if wraplib=yes"
	@echo "        Default value: . (that is, use current work directory)"
	@echo
	@echo "  MKLRUNLIB_PATH - Intel(R) MKL lib directory to be used to run examples"
	@echo "        in case of dynamic linkage"
	@echo "        Default value is current Intel(R) MKL libs located at"
	@echo '          $$(MKLROOT)/lib'
	@echo
	@echo "  LIB_PATH - If you are using pgi threading layer,"
	@echo "        specify path to libraries: libpgmp"
	@echo "        or add needed path to DYLD_LIBRARY_PATH"
	@echo
	@echo "N.B. To run examples on some specific target architecture"
	@echo "  add needed options to make-arguments: TARGET_ARCH=..."
	@echo "  and refer to GNU, PGI and Intel compilers documentation"
	@echo "  for more details about target architecture specific options"
	@echo

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

include fftw3xf.lst

function = $(DFT)
compiler = intel
INTERFACE = lp64
threading = parallel
wraplib = no

wraplibdir = $(INSTALL_DIR)/wrap_lib$(_ilp64)_$(_IA)

##-----------------------------------------------------------------------------
## Main targets

.PHONY: libem64t libintel64 dylibem64t dylibintel64 \
	report clean cleanup FORCE

libintel64 libem64t:
	$(MAKE) -f $(MY_MAKEFILE) prepare_run _IA=intel64 _LIB=lib
	$(MAKE) -f $(MY_MAKEFILE) run _IA=intel64 _LIB=lib

dylibintel64 dylibem64t:
	$(MAKE) -f $(MY_MAKEFILE) prepare_run _IA=intel64 _LIB=dylib
	$(MAKE) -f $(MY_MAKEFILE) run _IA=intel64 _LIB=dylib

report: FORCE
	@echo
	@echo "--------------------------------------------------"
	@echo There are `ls source/*.f* | wc -l` \
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
	-rm -rf $(INSTALL_DIR)/wrap_lib* # clean all built wrap_libs

FORCE: ;

ifdef VERBOSE
  $(info )
  $(info compiler=$(compiler) INTERFACE=$(INTERFACE) threading=$(threading) \
         parallel=$(parallel) omp=$(omp) wraplib=$(wraplib))
  $(info )
endif

ifdef _IA
##-----------------------------------------------------------------------------
## Supporting _macros

_FC_intel = ifort
_FC_gnu = gfortran
_FC_pgi = pgf95
FC = $(firstword $(_FC_$(compiler)) ifort)

_ilp64 = _$(INTERFACE)

# Define _fflags
_fflags = $(_fflags_$(compiler))
_fflags += $(_fflags_$(compiler)_$(_IA))
_fflags += $(_fflags_$(compiler)_$(INTERFACE))
_fflags += $(_fflags_$(compiler)_$(parallel))
_fflags += $(_fflags_$(compiler)_diag)
_fflags += -I$(MKLROOT)/include/fftw
_fflags += $(FFLAGS) $(TARGET_ARCH)

_fflags_intel =
_fflags_intel_ilp64 = -i8
_fflags_intel_diag = -warn all -warn errors

_fflags_gnu = -fcray-pointer
_fflags_gnu_ilp64 = -fdefault-integer-8
_fflags_gnu_intel64 = -m64
_fflags_gnu_diag = -Wall -Werror

_fflags_pgi = -Mnokeepobj -Wl,-no_compact_unwind
_fflags_pgi_ilp64 = -i8
_fflags_pgi_pgi = -mp

# Maybe define wraplib
ifeq ($(wraplib),yes)
  _maybe_wrap = _with_wraplib
  _maybe_wraplib = $(wraplibdir)/libfftw3xf.a
  _maybe_wraplib_libs = -L$(wraplibdir) -lfftw3xf
  _maybe_wraplib_makeflags_ilp64 = INTERFACE=ilp64
endif

# Pick Intel(R) MKL interface layer
_lib_iface = mkl_intel$(_ilp64)

# Pick Intel(R) MKL threading layer
ifeq ($(threading),parallel)
  parallel = $(_parallel_$(compiler))
  omp = $(_omp_$(parallel))

  _parallel_intel = intel
  _parallel_gnu = intel# gnu missing
  _parallel_pgi = pgi

  _omp_intel = iomp5
  _omp_gnu = iomp5# gomp missing
  _omp_pgi = pgmp

  _lib_thread = mkl_$(parallel)_thread
  threadlayer = $(parallel)
else
  _lib_thread = mkl_sequential
  threadlayer = $(threading)
endif

ifeq ($(compiler)-$(parallel)-$(omp),intel--tbb)
  override _lib_thread = mkl_sequential
endif

# Pick Intel(R) MKL core layer
_lib_core = mkl_core

_threading = $(threading)
# Maybe pick OpenMP library
ifeq ($(threading),parallel)
  ifneq ($(omp),)
    _maybe_omp = -L$(CMPLR_PATH) -L$(TBB_PATH) -l$(omp)
    _threading = $(threading)_$(omp)
  endif
endif


CMPLR_PATH = $(MKLROOT)/../compiler/lib
TBB_PATH = $(MKLROOT)/../tbb/lib

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

# Finally compose the _libs
_libs = $(_maybe_wraplib_libs)
ifeq ($(_LIB),dylib)
  _libs += -L$(MKLROOT)/lib
  ifneq ($(MKL_LIBS_LINK),mixed)
    _libs += -lmkl_rt
  else
    _libs += -l$(_lib_iface) -l$(_lib_thread) -l$(_lib_core)
  endif
else
  _libs += $(MKLROOT)/lib/lib$(_lib_iface).a
  _libs += $(MKLROOT)/lib/lib$(_lib_thread).a
  _libs += $(MKLROOT)/lib/lib$(_lib_core).a
endif
_libs += $(_maybe_omp)
_libs += -lm -lpthread

RUNENV = DYLD_BIND_AT_LAUNCH=1

ifneq ($(LIB_PATH),)
  RUNENV += DYLD_LIBRARY_PATH=$(LIB_PATH):$(DYLD_LIBRARY_PATH)
endif

ifeq ($(_LIB),dylib)
  RPATH = -Wl,-rpath,$(MKLRUNLIB_PATH) -Wl,-rpath,$(CMPLR_PATH) -Wl,-rpath,$(TBB_PATH)
  ifneq ($(MKL_LIBS_LINK),mixed)
    RUNENV += MKL_INTERFACE_LAYER=$(INTERFACE) MKL_THREADING_LAYER=$(threadlayer)
  endif
else
  RPATH = -Wl,-rpath,$(CMPLR_PATH)
endif

_resdir = $(RES_DIR)/$(compiler)$(_ilp64)_$(_threading)_$(_LIB)$(_IA)$(_maybe_wrap)

##-----------------------------------------------------------------------------
## Rules

vpath %.f   source
vpath %.f90 source

.SUFFIXES:
.SUFFIXES: .f .f90 .o .res .out

.PHONY: prepare_run run mkresdir wraplib

prepare_run: mkresdir wraplib

mkresdir:
	@echo
	@echo See results in $(_resdir)
	@echo
	test -d $(_resdir) || mkdir -p $(_resdir)

wraplib: $(_maybe_wraplib)

ifeq ($(wraplib),yes)
# We shall use absolute path for install_to
_install_to=$(shell mkdir -p $(wraplibdir); cd $(wraplibdir) && pwd)
$(_maybe_wraplib):
	@echo
	# build $@
	$(MAKE) -C $(MKLROOT)/interfaces/fftw3xf lib$(_IA) \
	  INSTALL_DIR=$(_install_to) INSTALL_LIBNAME=$(@F) \
	  $(_maybe_wraplib_makeflags$(_ilp64))
endif

run: $(function:%=$(_resdir)/%.res)

$(_resdir)/%.res: $(_resdir)/%.out FORCE
	-rm -f $@ # remove $@ file before run
	@echo
	# run $*.out
	$(RUNENV) \
	  $< > $@

.PRECIOUS: $(_resdir)/%.out

$(_resdir)/%.out: %.f90
	@echo
	# compile $*.f90
	$(FC) $(_fflags) $^ $(RPATH) $(_libs) -o $@

##-----------------------------------------------------------------------------
endif
