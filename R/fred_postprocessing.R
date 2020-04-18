#' calculate_fred_R0
#'
#' Calculate the R(t) for a FRED_JOB
#' 
#' @param fred_key key of the FRED job
#' @param fred_n number of replicates in FRED job
#' @return doesn't return anything. It writes the output to two files
#' @export
#' @examples
#' calculate_fred_R0(test_job, 10)
calculate_fred_R0 <- function(fred_key, fred_n){
    data_dir = file.path(system(sprintf("fred_find -k %s", fred_key), intern = TRUE),
                         "DATA", "OUT")
    
    params_file = file.path(system(sprintf("fred_find -k %s", fred_key), intern = TRUE),
                            "META", "PARAMS")
    
    sim_days = unlist(str_match(system(sprintf("cat %s | grep -E \"days +=\"", params_file), intern = TRUE),
                                'days\\s+=\\s+([0-9]+)'))[2]
    sim_days = as.integer(sim_days)
    
    days_df = data.frame(Day = 0:(sim_days-1), stringsAsFactors = F)
    weeks_df = data.frame(Week = 1:floor(sim_days/7), stringsAsFactors = F)
    
    RR_df = tibble()
    RR_weekly = tibble()
    
    for(nn in 1:fred_n){
        RR_tmp =  calculate_R0(data_dir, nn)
        if(nrow(RR_tmp$Rt_df) == 0){
            write_csv(tibble(Day = 1:sim_days,Rt_mean = 0, Rt_std = 0), path=file.path(data_dir,"Rt_estimates_weekly.csv"))
            write_csv(tibble(Day = 1:sim_days,Rt_mean = 0, Rt_std = 0), path=file.path(data_dir,"Rt_estimates_daily.csv"))
            return(tibble(Day = 1:sim_days,Rt_mean = 0, Rt_std = 0))
        }
        RR_df = bind_rows(RR_df, RR_tmp$Rt_df)
        RR_weekly = bind_rows(RR_weekly, RR_tmp$Rt_weekly)    
    }
    
    RR_df %<>%
        group_by(day) %>%
        summarize(Rt_mean = mean(Rt, na.rm = T),
                  Rt_std = sd(Rt, na.rm = T)) %>%
        rename(Day = day) %>%
        ungroup() %>% 
        right_join(days_df, by = "Day") %>%
        replace_na(list(Rt_mean = 0, Rt_std = 0))
    
    RR_weekly %<>%
        group_by(Week) %>%
        summarize(Rt_mean = mean(Rt, na.rm = T),
                  Rt_std = sd(Rt, na.rm = T)) %>%
        ungroup() %>%
        right_join(weeks_df, by = "Week") %>%
        replace_na(list(Rt_mean = 0, Rt_std = 0))
    
    write_csv(RR_weekly, path=file.path(data_dir,"Rt_estimates_weekly.csv"))
    write_csv(RR_df, path=file.path(data_dir,"Rt_estimates_daily.csv"))
    return(RR_df)
}
#' calculate_R0
#'
#' Calculate the R(t) for a FRED_JOB with data folder specified and for one run
#' 
#' @param data_dir_in directory where the output is
#' @param n_in replicate of the FRED job to calculate R0 over
#' @return returns a timeseries of R(t)
#' @export
#' @examples
#' calculate_R0('FRED_RESULTS/JOB/1/DATA/OUT/', 1)
calculate_R0 <- function(data_dir_in, n_in){
    infections = read_delim(file=file.path(data_dir_in, sprintf("infections%d.txt",n_in)),col_names = F, delim = " ")
    if(nrow(infections) == 0){
        return(list(Rt_df = tibble(), Rt_weekly = tibble()))
    }
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


#' calculate intervals
#' calculate serial intervals and infectious periods
#' 
#' @param fred_key key of fred job
#' @param fred_n replicate of the FRED job to calculate R0 over
#' @return a list of dataframes: periods has the inf and symp periods, intervals the serial intervals
#' @export
#' @examples
#' calculate_intervals('FRED_RESULTS/JOB/1/DATA/OUT/', 1)
calculate_intervals <- function(fred_key, fred_n){
    ## calculate infectious period
    data_dir = file.path(system(sprintf("fred_find -k %s", fred_key), intern = TRUE),
                         "DATA", "OUT")        
    params_file = file.path(system(sprintf("fred_find -k %s", fred_key), intern = TRUE),
                            "META", "PARAMS")

    infections = readr::read_delim(file=file.path(data_dir, sprintf("infections%d.txt",fred_n)),col_names = F, delim = " ",col_types = cols(.default = "c"))

    if(nrow(infections) == 0){
        return(list(periods = tibble(), intervals = tibble()))
    }
    
    conn_inf = file(file.path(data_dir, sprintf("infections%d.txt",fred_n)), "r")
    inf_lines = readLines(conn_inf)
    close(conn_inf)
    
    cols_req = c('day', 'host','age','infector')        
    cols_vec = c('inf','symp')
    names_periods = c('inf1', 'inf2','symp1','symp2')
    

    ## Only simulate MSA areas with all counties in pop
    infections = data.frame(stringsAsFactors=F)

    infections = sapply(1:length(inf_lines), function(x){
        inf_list = str_split(inf_lines[x], pattern="\\s+")[[1]]
        names_ind = which(inf_list %in% cols_req) + 1
        names_vec = rep(which(inf_list %in% cols_vec),each=2) + rep(c(1,2),2)
        tmp_df = inf_list[c(names_ind,names_vec)]
        return(tmp_df)
    })
    infections = t(infections)
    names(infections) = c(cols_req, names_periods)
    infections = as.data.frame(infections, stringsAsFactors = F)
    colnames(infections) = c(cols_req, names_periods)
            
    infections_df = infections[,cols_req]
    infections_df$day = as.numeric(infections_df$day)
    infections_df$age = floor(as.numeric(infections_df$age))
    
    periods_df = infections[,names_periods]
    infections_df$symp = as.numeric(periods_df$symp1)
    periods_df$host = infections_df$host
    periods_df$age = infections_df$age
    
    periods_df = mutate(periods_df,
                        inf1 = as.numeric(inf1), inf2 = as.numeric(inf2),
                        symp1 = as.numeric(symp1), symp2 = as.numeric(symp2)) %>%
        filter(inf1 != -1 & inf2 != -1 & symp1 != -1 & symp2 != -1) %>%
        mutate(inf_period = inf2 - inf1, symp_period = symp2 - symp1) %>%
        dplyr::select(host,age,inf_period, symp_period)
    
    intervals_df = left_join(infections_df, infections_df,
                             by = c("infector" = "host"), suffix = c('','_infector')) %>%
        filter(infector != "-1", symp != -1, symp_infector != -1) 
    return(list(periods = periods_df, intervals = intervals_df))
}


#' calculate CF intervals
#' calculate delays of CF from symptoms and infection
#' 
#' @param fred_key key of fred job
#' @param fred_n replicate of the FRED job to calculate R0 over
#' @return a list of dataframes: periods has the inf and symp periods, intervals the serial intervals
#' @export
#' @examples
#' calculate_CF_intervals('test_cf_intervals', 1)
calculate_CF_intervals <- function(fred_key, fred_n){
    ## calculate infectious period
    data_dir = file.path(system(sprintf("fred_find -k %s", fred_key), intern = TRUE),
                         "DATA", "OUT")        
    
    cols_req = c('day', 'age', 'exp', 'inf', 'symp','dead')

    
    conn_inf = file(file.path(data_dir, sprintf("infectionsCF%d.txt",fred_n)), "r")
    inf_lines = readLines(conn_inf)
    close(conn_inf)

    if(length(inf_lines) == 0){
        return(tibble())
    }
    infections_cf = data.frame(stringsAsFactors=F)
    infections_cf = sapply(1:length(inf_lines), function(x){
        inf_list = str_split(inf_lines[x], pattern="\\s+")[[1]]
        names_ind = which(inf_list %in% cols_req) + 1
        tmp_df = inf_list[c(names_ind)]
        return(tmp_df)
    })
    infections_cf = t(infections_cf)
    names(infections_cf) = c(cols_req)
    infections_cf = as.data.frame(infections_cf, stringsAsFactors = F)
    colnames(infections_cf) = c(cols_req)    
    
    infections_df = infections_cf[,cols_req]
    infections_df$day = as.numeric(infections_df$day)
    infections_df$age = floor(as.numeric(infections_df$age))
    infections_df$exp = as.numeric(infections_df$exp)
    infections_df$inf = as.numeric(infections_df$inf)
    infections_df$symp = as.numeric(infections_df$symp)
    infections_df$dead = as.numeric(infections_df$dead)
    
    return(infections_df)
}
