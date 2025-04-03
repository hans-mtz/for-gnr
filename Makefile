FC = ifort
FFLAGS = -O3 -qopenmp  -heap-arrays 20 -mkl -lmkl_blas95_lp64 -lmkl_lapack95_lp64 -lmkl_intel_lp64 -I/usr/local/include -L/usr/local/lib -L/opt/intel/oneapi/mkl/2021.1.1/lib -lnlopt
DFLAGS = -g -debug all -qopenmp -heap-arrays 20 -O0 -traceback -check all -init=snan -check bounds -check uninit -L/opt/intel/oneapi/mkl/2021.1.1/lib -mkl -lmkl_blas95_lp64 -lmkl_lapack95_lp64 -lmkl_intel_lp64 -L/usr/local/lib -lnlopt
prefix = /Users/hans/F_code_mkl
FLAGS = $(FFLAGS)
SRC_DIR = Code/modules
SRC_FILES = $(wildcard $(SRC_DIR)/*.f90)
OBJ_DIR = Code/modules
OBJ_FILES = $(patsubst $(SRC_DIR)/%.f90,$(OBJ_DIR)/%.o,$(SRC_FILES))
MAIN_DIR = Code
MAIN_FILES = $(wildcard $(MAIN_DIR)/main*.f90)
MAIN_OBJS = $(MAIN_FILES:.f90=.o)
EXECUTABLES = $(MAIN_FILES:$(MAIN_DIR)/%.f90=%)

all: $(EXECUTABLES)

$(EXECUTABLES): %: $(MAIN_DIR)/%.o $(OBJ_FILES)
	$(FC) $(FLAGS) -o $@ $^

$(OBJ_DIR)/%.o: $(SRC_DIR)/%.f90
	$(FC) -c $(FLAGS) -o $@ $<

$(MAIN_DIR)/%.o: $(MAIN_DIR)/%.f90
	$(FC) -c $(FLAGS) -o $@ $<

# Include dependencies main on gnr_mod, gnr_mod on nlopt etc
# use gendeps?

$(MAIN_OBJS) : $(OBJ_DIR)/globvar.o $(OBJ_DIR)/init.o $(OBJ_DIR)/gnr_mod.o $(OBJ_DIR)/io.o

# $(OBJ_DIR)/init.o : $(OBJ_DIR)/globvar.o $(OBJ_DIR)/io.o

$(OBJ_DIR)/gnr_mod.o : $(OBJ_DIR)/nrutil.o $(OBJ_DIR)/stats.o $(OBJ_DIR)/matrix.o $(OBJ_DIR)/globvar.o $(OBJ_DIR)/nlopt.o

$(OBJ_DIR)/stats.o : $(OBJ_DIR)/intel_random.o

$(OBJ_DIR)/globvar.o : $(OBJ_DIR)/init.o $(OBJ_DIR)/io.o

lib: gnr-r-for.so

$(MAIN_DIR)/gnr-r-for.so: $(MAIN_DIR)/gnr-r-for.f90 $(OBJ_FILES)
	$(FC) -fpic -shared $(FLAGS) $^ -o $@

echo:
	echo $(OBJ_FILES) $(EXECUTABLES) $(MAIN_OBJS)

clean:
	rm -f $(OBJ_DIR)/*.o $(OBJ_DIR)/*.mod $(EXECUTABLES) Code/*.o Code/*.mod *.mod main debug

debug: FLAGS = $(DFLAGS)
debug: $(EXECUTABLES)

.PHONY: clean debug echo lib