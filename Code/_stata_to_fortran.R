# Loading packages ---------------------------
library(tidyverse)
library(parallel)
library(modelsummary)
library(tinytable)

# Reading data -------------------------------
stata_dta<-haven::read_dta("Code/GNR-ado/Replication_files/Tables_2_3/Colombia/Industry/data_col.dta")
data_foreign<-foreign::read.dta("Code/GNR-ado/Replication_files/Tables_2_3/Colombia/Industry/data_col.dta")
stata_dta |> View()
data_foreign |> View()
stata_dta %>% datasummary_skim(
    fun_numeric = list(
        `Missing (%)` = PercentMissing, #(x)sum(!is.na(x))/length(x), 
        Mean = Mean, 
        SD = SD, 
        Min = Min,
        Q1 = P25,
        Median = Median,
        Q3 = P75,
        Max = Max
    ),
    # fmt = 3,
    # output = 'kableExtra',
    # table.attr = 'data-quarto-disable-processing="true"'
    )

stata_dta %>%
    summarise(
        zeros = sum(ifelse(RI<=0,1,0), na.rm = TRUE)
    )
# Data Wrangling -----------------------------

stata_out<-stata_dta %>%
            mutate(
                sic= as.numeric(str_sub(as.character(id), 1, 3)),
                plant=as.numeric(id),
                .before = logRGO
            ) %>%
            select(
                sic,
                plant,
                year,
                y=logRGO,
                s=si,
                m=logRI,
                k=logK,
                l=logL
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
            )

evasion_inds<-c(322,324, 342, 313, 321, 351)
gnr_inds<-c(311,321,322,331,381)

# Saving data for Fortran to read----------------
lapply(
    union(gnr_inds, evasion_inds),
    function(x){
        (temp_data<-stata_out %>%
            filter(sic==x) %>%
            select(-sic)
        )
        (temp_data<- temp_data[complete.cases(temp_data),])
        print(dim(temp_data))
        write_delim(
            temp_data,
            paste0("Data/stata_data_",x,".raw"),
            append = FALSE,
            col_names = FALSE
        )
        variable_names<-names(temp_data)
        write.table(
            variable_names,
            paste0("Data/stata_variables_",x,".raw"),
            col.names = FALSE,
            row.names = FALSE
            # append = FALSE
        )
    }
)
