load("Code/gnrprod-master/data/colombian.rda")
library(tidyverse)

to_fortran<-colombian %>%
    select(
        plant=id,
        year=year,
        y=RGO,
        s=share,
        m=RI,
        k=K,
        l=L
    ) %>%
    group_by(plant) %>%
    mutate(
        ll=l*l,
        mm=l*m,
        mk=m*k,
        ml=m*l,
        kk=k*k,
        kl=k*l,
        klm=k*l*m
        # across(
        #     everything(),
        #     ~lag(.x, order_by = year),
        #     .names = "lag_{.col}"
        # )
    ) %>%
    select(
        plant:klm#, lag_k, lag_m, lag_l:lag_kl, ,lag_y, lag_s
    )

to_fortran# |> names()
to_fortran<-to_fortran[complete.cases(to_fortran), , drop=FALSE]
to_fortran# |> head()
write_delim(
    to_fortran,
    "Data/colombia_R.raw",
    append = FALSE,
    col_names = FALSE
)
variable_names<-names(to_fortran)
write.table(
    variable_names,
    "Data/variables_R.raw",
    col.names = FALSE,
    row.names = FALSE
    # append = FALSE
)
dim(to_fortran)

## GNR R-Fortran
R CMD SHLIB -o gnr-r-for.so gnr-r-for.f90 modules/gnr_mod.f90 modules/nrutil.f90 modules/stats.f90 modules/matrix.f90 modules/globvar.f90 -L/opt/intel/oneapi/mkl/2021.1.1/lib -lmkl_blas95_lp64 -lmkl_lapack95_lp64 -mkl -nlopt 

# Flex
# dyn.load("/opt/intel/oneapi/mkl/2021.1.1/lib/libmkl_blas95_lp64.a")
# dyn.load("/opt/intel/oneapi/mkl/2021.1.1/lib/libmkl_intel_lp64.1.dylib")
dyn.load("Code/gnr-r-for.so")

# require("dotCall64")
library(dotCall64)
s<-to_fortran$s
D<-to_fortran %>% ungroup () %>% select(-s,-y,-plant,-year) |> as.matrix()
# flex_e <-s
flex_e <-rep(0,length(s))
gam <- rep(0,ncol(D))
test_out<-.C64("gnr_flex",
    SIGNATURE=c(rep("double",4),rep("int64",2)),
    s=s,D=D,flex_e=flex_e,gam=gam,n=nrow(D),m=ncol(D),
    INTENT=c(rep("r",2),rep("w",2),rep("r",2))
)
