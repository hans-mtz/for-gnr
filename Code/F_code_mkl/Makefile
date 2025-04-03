# Makefile to create library from Salvador's files link by adding -lmylib
FC = ifort
FFLAGS = -O3 -mkl -m64 -lmkl_blas95_ilp64 -lmkl_blas95_lp64 \
	-lmkl_lapack95_ilp64 -lmkl_lapack95_lp64 -xHost #-init=snan #-gen-dep=deps.txt
DFLAGS = -g3 -check all -init=snan
# prefix = /Users/hans/F_code_mkl
# prefix1 = /opt/intel/oneapi/mkl/2021.1.1/include
SRC = nrutil.f90 \
	integration.f90 \
	random.f90 \
	probability.f90 \
	minimization.f90 \
	anneal.f90 \
	GN.f90 \
	matrix.f90 \
	quantal.f90 \
	ars.f90	\
	nonparametric.f90	\
	simplex.f90 \
	statistics.f90 \
	interpol.f90 \
	module_csv.f90 \
	io.f90 \
	# sobol.f90 \
	# pars.f90
	# globvar.f90 \
	# init.f90 \
	# likelihood_evaluation.f90 \
	# likelihood.f90 \
	# io.f90 \
	# normal.f90 \
	# main.f90

# OBJ = ${SRC:.f90=.o}

# %.o: %.f90
# 	$(FC) -c $(FFLAGS) -o $(@F) $<

mylib: $(SRC)
	$(FC) -dynamiclib -fpic  -o lib$@.dylib $(FFLAGS) $(SRC)

# debug:
# 	$(FC) $(DFLAGS) $(FFLAGS) -o $@ $(notdir $(OBJ))

clean:
	rm -f *.o *.mod *.out *.dylib mylib
