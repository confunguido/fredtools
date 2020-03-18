#!/usr/bin/env Rscript
##=======================================#
## Author: Guido Espana
## Calculates the R0 from FRED output
##=======================================#
## Setup---------------
##=======================================#
library(tidyverse)
fred_key = "FRED_R0_calib_flu_10"
fred_n = 10
    
args = (commandArgs(TRUE))
if(length(args) >= 1){
    fred_key = args[1]
    if(length(args) >= 2){
        fred_n = as.numeric(args[2])
    }
}


##=======================================#
## Calculate Rt------------
##=======================================#
calculate_R0 <- function(data_dir_in, n_in){
        infections = read_delim(file=file.path(data_dir_in, sprintf("infections%d.txt",n_in)),col_names = F, delim = " ")

        cols_req = c('day', 'host','infector')
        names_ind = which(as.character(infections[1,]) %in% cols_req) + 1
        
        infections_df = infections[,names_ind]
        colnames(infections_df) = cols_req
        ## 1. Find all the infected IDs
        ## 2. Count infections per infected
        infector_df = infections_df %>%
            group_by(infector) %>%
            summarize(NewInf = n()) %>%
            ungroup() %>%
            rename(host = infector) %>%
            right_join(infections_df, by = c("host" = "host")) %>%
            replace_na(list(NewInf = 0))
        
        ## 3. Group the infections per infected (R) for each day
        Rt_df = infector_df %>% group_by(day) %>%
            summarize(NewInfs = sum(NewInf),
                      hosts = n()) %>%
            ungroup() %>%
            mutate(Rt = NewInfs / hosts,
                   run = n_in)
        
        Rt_weekly = Rt_df %>%
            mutate(Week = floor(day / 7)) %>%
            group_by(Week) %>%
            summarize(Rt = mean(Rt)) %>%
            ungroup() %>% mutate(run = n_in)
        
        return(list(Rt_df = Rt_df, Rt_weekly = Rt_weekly))
}


##=======================================#
## find job and read read data------------
##=======================================#
data_dir = file.path(system(sprintf("fred_find -k %s", fred_key), intern = TRUE),
                     "DATA", "OUT")

RR_df = tibble()
RR_weekly = tibble()
for(nn in 1:fred_n){
    RR_tmp =  calculate_R0(data_dir, nn)
    RR_df = bind_rows(RR_df, RR_tmp$Rt_df)
    RR_weekly = bind_rows(RR_weekly, RR_tmp$Rt_weekly)
    
}
RR_df %<>%
    group_by(day) %>%
    summarize(Rt_mean = mean(Rt, na.rm = T),
              Rt_std = sd(Rt, na.rm = T)) %>%
    rename(Day = day) %>%
    ungroup() %>% replace_na(list(Rt_std = 0))

RR_weekly %<>%
    group_by(Week) %>%
    summarize(Rt_mean = mean(Rt, na.rm = T),
              Rt_std = sd(Rt, na.rm = T)) %>%
    ungroup() %>% replace_na(list(Rt_std = 0))

write_csv(RR_weekly, path=file.path(data_dir,"Rt_estimates_weekly.csv"))
write_csv(RR_df, path=file.path(data_dir,"Rt_estimates_daily.csv"))

