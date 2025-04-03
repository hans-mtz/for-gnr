## GNR in FORTRAN

This repository contains a FORTRAN implementation of the production function estimation method in Gandhi, Navarro, Rivers (2020) "On the Identification of Gross Output Production Functions".

### Requirements

- intel fortran compiler
- intel fortran libraries (mkl, mkl-vsl)
- NLOPT library for optimization

### Usage

I include a makefile to compile the code. By running `make`, the code will be compiled and will create two executables: `mainNP` and `mainCD`.

The `mainNP` executable runs the non-parametric estimation of the production function, while the `mainCD` executable runs the Cobb-Douglas functional form of the production function.

The code is organized in modules in the `Code/modules` folder.

The data is read from a raw file in the `Code/data` folder. The data is in a raw format, and the code reads it using the `read_data` subroutine in the `Code/modules/read_data.f90` module. The data is then passed to the `mainNP` or `mainCD` subroutines for estimation.

The program expects the data to be arranged by plant and year, and the variables to be in the following order:

- plant
- year
- y (gross output)
- s (log intermediates' share of revenue)
- m (log intermediates)
- k (log capital)
- l (log labour)
- ll
- mm
- mk
- ml
- kk
- kl
- klm

Both routines expect all of these variables to be present in the data. The `mainCD` routine also expects the `ll`, `mm`, `mk`, `ml`, `kk`, `kl`, and `klm` variables to be present in the data, even though they are NOT used.

The results are saved in a text file in the `Code/Data` folder. Two files are created: one for the estimated coefficients of the CD production function and a second one for the estimated productivity.

### Notes

The code has been tested on a MacOS system with the Intel Fortran compiler. 

