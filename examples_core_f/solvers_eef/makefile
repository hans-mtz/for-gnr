#===============================================================================
# Copyright 2004-2020 Intel Corporation.
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
##     Intel(R) Math Kernel Library Extended Eigensolver Fortran examples makefile
##******************************************************************************

include solvers_eef.lst

define usage

Usage:
------
  #> make Target                          \
          [interface=interface_name]      \
          [threading=parallel|sequential] \
          [MKLROOT=path]                  \
          [example=name]

  Targets:

    libintel64   - statically linked example for processors that use the Intel(R) 64 architecture"
    dylibintel64 - dynamically linked example for processors that use the Intel(R) 64 architecture"
    help         - print this help"

  Options:

    interface=interface_name
                - Can be lp64 (default) or ilp64 for intel64.

    threading=parallel
        - Use Intel(R) MKL in the threaded mode, default.
    threading=sequential
        - Use Intel(R) MKL in the sequential mode

    MKLROOT=<MKL_directory>
        - Specifies the location of Intel(R) MKL libraries used to build this example.
          Default: the Intel(R) MKL installation directory.

    example="example1 example2 ..."
        - Specifies the list of examples to run (source name without extension).
          Proper values are:
          $(SOLVERS_EE)
          All examples will be executed by default.

Usage examples:
--------------
  #> make dylibintel64 interface=ilp64 threading=sequential
        - Link all the examples against sequential dynamic MKL-Intel64
          using ILP64 interface.

endef

help: export usage := $(usage)
help: ; @echo "$$usage"

# Main targets
libintel64:   ; $(MAKE) ext=a  _ia=intel64 run
dylibintel64: ; $(MAKE) ext=so _ia=intel64 run
rtintel64:    ; $(MAKE) ext=rt _ia=intel64 run

.PHONY: clean
clean:
	-rm -rf _results;

# Check options and set default values
#   call args: $1 - option name, $2 - default value, $3 - other appropriate values for the option
check-opt = \
    $(if $(value $(strip $1)),,$(eval override $1 = $2)) \
    $(if $(filter $2 $3, $(value $(strip $1))),, \
        $(error [ERROR] Incorrect option value: '$(strip $1)=$(value $(strip $1))'! Should be some of '$(strip $2 $3)'))
$(call check-opt, threading, parallel, sequential)
$(call check-opt, example, $(SOLVERS_EE))
$(call check-opt, interface, lp64, ilp64)
$(call check-opt, MKLROOT, ../.., %)
compiler=intel

# Paths to Intel(R) MKL libraries
IMKL_PATH  = "$(MKLROOT)/lib"
IOMP_PATH  = "$(MKLROOT)/../compiler/lib"

# Vendor specific compiler options
FOPTS.intel = $(if $(iface.ilp64),-i8) -warn all -module $(RES_DIR) -fpp
FOPTS.intel.intel64 = -m64 # This option tells compiler to generate code for Intel64 architecture.

FOPTS = $(FOPTS.$(compiler).$(_ia)) $(FOPTS.$(compiler)) $(FOPTS.$(_ia))

# Macro to use in conditions
iface.$(interface) = $(interface)
platform.$(_ia) = $(_ia)

# Linker options
LINK_OPTS = $(IMKL_LIBS.$(ext)) -L $(IOMP_PATH) -liomp5 -lm

# Intel(R) MKL libraries to use in linking
IMKL_LIBS    = $(iface_lib.$(compiler))$(suffix_lib.$(_ia)) mkl_core mkl_$(compiler)_thread

IMKL_LIBS.a  = $(IMKL_LIBS:%=$(IMKL_PATH)/lib%.a)
IMKL_LIBS.so = -L $(IMKL_PATH) $(IMKL_LIBS:%=-l%)
IMKL_LIBS.rt = -L $(IMKL_PATH) -lmkl_rt

iface_lib.$(compiler) = mkl_intel
suffix_lib.$(_ia)     = _$(interface)

# Compiler tools
RPATH = -Wl,-rpath,$(IMKL_PATH) -Wl,-rpath,$(IOMP_PATH)
FC.intel = ifort

FC.comp = $(FC.$(compiler)) $(FOPTS) $(RPATH) -I$(MKLROOT)/include -w $(FOPTS.f.$(compiler))

#-------------------------------------------------------------------------------

RES_DIR = _results/$(compiler)_$(_ia)$(if $(platform.intel64),_$(interface))_$(threading)_$(ext)
RES = $(example:%=$(RES_DIR)/%.res)

run   : $(RES)
$(RES): .FORCE
.FORCE:
.PHONY: run

.NOTPARALLEL: # as written, this can only be run with make -j1, to support creation of .mod files first

$(RES_DIR)/%.res: $(RES_DIR)/%.exe ; $(shell which env) $< > $@

MODULE_SPBLAS = $(RES_DIR)/mkl_spblas.o
MODULE_SOLVERS_EE = $(RES_DIR)/mkl_solvers_ee.o

.SECONDARY:
$(RES_DIR)/%.exe: $(RES_DIR)/%.o $(MODULE_SPBLAS) $(MODULE_SOLVERS_EE); $(FC.comp) $^ -o $@ $(LINK_OPTS)

$(RES_DIR)/%.o: source/%.f90 $(MODULE_SPBLAS) $(MODULE_SOLVERS_EE) | $(RES_DIR)/. ;	$(FC.comp) $< -c -o $@

$(RES_DIR)/%.o: source/%.f | $(RES_DIR)/. ; $(FC.comp) $< -c -o $@

$(MODULE_SPBLAS): $(MKLROOT)/include/mkl_spblas.f90 | $(RES_DIR)/. ; $(FC.comp) $< -c -o $@

$(MODULE_SOLVERS_EE): $(MKLROOT)/include/mkl_solvers_ee.f90 | $(RES_DIR)/. ; $(FC.comp) $< -c -o $@

%/. : ; mkdir -p $@
