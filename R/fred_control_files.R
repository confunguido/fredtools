#' write_fred_parameters
#' 
#' @param scalars.in data frame with parameters
#' @param defaults.file file with default parameters
#' @param dir.in directory where the simulation files will be saved
#' @param basename.in basename for the file names
#' @return returns 0 if ERROR
#' @export
#' @examples
#' write_fred_parameters(scalars, 'params.txt', 'simfiles','fred_params')
write_fred_parameters = function(scalars.in, defaults.file, dir.in, basename.in='fred_params', fred_defaults='./FRED/input_files/defaults'){    
    ## read default values from defaults.file
    conn = file(defaults.file, "r")
    default_lines = readLines(conn)
    close(conn)
    defaults_df = data.frame(stringsAsFactors=F)
    for(n in 1:length(default_lines)){
        param_split = unlist(str_split(default_lines[n], pattern = "="))
        if(length(param_split) == 2){
            tmp_df = data.frame(
                Param = str_trim(param_split[1], side = 'both'),                
                Value = str_trim(param_split[2], side = 'both'), stringsAsFactors = F
            )
        }else{
            tmp_df = data.frame(Param = default_lines[n], Value = '', stringsAsFactors = F)
        }
        defaults_df = rbind(defaults_df, tmp_df)
    }

    ## read default values from fred_defaults
    conn = file(fred_defaults, "r")
    default_lines = readLines(conn)
    close(conn)
    fred_defaults_df = data.frame(stringsAsFactors=F)
    for(n in 1:length(default_lines)){
        param_split = unlist(str_split(default_lines[n], pattern = "="))
        if(length(param_split) == 2){
            tmp_df = data.frame(
                Param = str_trim(param_split[1], side = 'both'),                
                Value = str_trim(param_split[2], side = 'both'), stringsAsFactors = F
            )
        }else{
            tmp_df = data.frame(Param = default_lines[n], Value = '', stringsAsFactors = F)
        }
        fred_defaults_df = rbind(fred_defaults_df, tmp_df)
    }
    
    ## modify the specified values 
    if(!dir.exists(dir.in)){
        dir.create(dir.in, recursive = TRUE)
    }
    in_template = colnames(scalars.in) %in% defaults_df$Param
    in_defaults = colnames(scalars.in) %in% fred_defaults_df$Param
    in_synthpop = "synthetic_population_id" %in% colnames(scalars.in)
    
    for(nn in 1:nrow(scalars.in)){
        params_file = sprintf("%s/%s_%i.txt",dir.in,basename.in, nn)
        
        if(file.exists(params_file)){
            file.remove(params_file)
        }
        file.connection = file(params_file, "a")
        for(k in 1:nrow(defaults_df)){
            kk = defaults_df$Param[k]
            if(str_detect(kk,'synthetic_population_id') && in_synthpop){
                next
            }
            if(defaults_df$Value[k] == ''){                
                write(defaults_df$Param[k], file.connection, append = TRUE)
            }else if(kk %in% colnames(scalars.in)){
                write(sprintf("%s = %s ", kk, as.character(scalars.in[nn,kk])),file.connection, append = TRUE)
            }else{
                write(sprintf("%s = %s ", kk, defaults_df$Value[k]),file.connection, append = TRUE)
            }
        }
        
        for(k in 1:length(in_template)){
            if(in_template[k] == FALSE && in_defaults[k] == TRUE){
                if(!(str_detect(colnames(scalars.in)[k],'synthetic_population_id') && in_synthpop)){
                    write(sprintf("%s = %s ", colnames(scalars.in)[k],
                                  as.character(scalars.in[nn,k])),file.connection, append = TRUE)
                }
            }
        }

        if(in_synthpop){
            write(scalars.in$synthetic_population_id[nn],file.connection, append = TRUE)
        }
        close(file.connection)
    }
}

#' Writes a submission script for a job array
#' This function takes specific parameters and creates the submission script that can be used later on to submit a job array of simulations
#' 
#' @param experiment_supername_in input the main name of the experiment, assuming there will be subexperiments for each submission
#' @param experiment_name_in input the name of the main experiment, would probably be the same as experiment_dir_in, except the experiment_dir_in can have a different path
#' @param experiment_dir_in input is the name of the main directory where the simulations will be located
#' @param params_base prefix of parameters file
#' @param job_base prefix of job key
#' @param reps repetitions for each job
#' @param n total simulations
#' 
#' @return a .sh file that is written to memory
#' @export
#' @examples
#' 
#' write_submission_array_slurm(
#'    executable_path_in = executable_path,
#'    input_files_path_in = input_files_path,
#'    experiment_supername_in = experiment_supername,
#'    experiment_name_in = experiment_name,
#'    experiment_dir_in = experiment_dir,
#'    unique_ID_vector = unique_SIM_IDs)
#' 
write_submission_array_slurm = function(experiment_supername_in,
                                  experiment_name_in,
                                  experiment_dir_in,
                                  params_base,
                                  job_base,
                                  reps, scalars, FUN, cores_in=1, walltime_in = "0:45:00",
                                  fred_home_dir_in="~/Coronavirus/FRED", fred_results_in="~/Coronavirus/FRED_RESULTS"){
    print('submit array')
    jobname = sprintf("%s-%s",experiment_supername_in, experiment_name_in)
    tmp_cmd_file = sprintf('tmp_execute_cmd_%s.txt',jobname)
    FUN(scalars, tmp_cmd_file)
    n = nrow(scalars)
    submission_template = "#!/bin/bash
#SBATCH --job-name=JOBNAME
#SBATCH --array=1-JOBSQUEUE
#SBATCH --nodes=1
#SBATCH --cpus-per-task=JOBCORES
#SBATCH --time=9:00:00                        # Time limit hrs:min:sec
#SBATCH --output=errors_job_%j.log            # Standard output and error log

#module load R/3.5.0
#cd $SLURM_WORKDIR

export FRED_HOME=FREDHOMESTR
export FRED_RESULTS=FREDRESULTSSTR
export PATH=${FRED_HOME}/bin:$PATH

file='TMPCMDFILE'
cmd=`head -n ${SLURM_ARRAY_TASK_ID} $file | tail -n 1`
cd EXPERIMENTDIR
eval $cmd
"
    submission_str = submission_template %>%
        str_replace_all(pattern="JOBNAME", replacement = jobname) %>%
        str_replace_all(pattern="EXPERIMENTDIR", replacement = experiment_dir_in) %>%
        str_replace_all(pattern="FREDHOMESTR", replacement = fred_home_dir_in) %>%
        str_replace_all(pattern="FREDRESULTSSTR", replacement = fred_results_in) %>%
        str_replace_all(pattern="JOBWALLTIME", replacement = walltime_in) %>%
        str_replace_all(pattern="JOBSQUEUE", replacement = as.character(n)) %>%
        str_replace_all(pattern="PARAMSBASE", replacement = params_base) %>%
        str_replace_all(pattern="JOBBASE", replacement = job_base) %>%
        str_replace_all(pattern="REPS", replacement = as.character(reps)) %>%
        str_replace_all(pattern="TMPCMDFILE", replacement = tmp_cmd_file) %>%
        str_replace_all(pattern="JOBCORES", replacement = as.character(cores_in))

    submission_file = sprintf("%s-%s.sh",experiment_supername_in,experiment_name_in)
    file.connection = file(submission_file)
    write(submission_str,file.connection)
    close(file.connection)
    return(submission_file)
}


#' Writes a submission script for a job array
#' This function takes specific parameters and creates the submission script that can be used later on to submit a job array of simulations
#' 
#' @param experiment_supername_in input the main name of the experiment, assuming there will be subexperiments for each submission
#' @param experiment_name_in input the name of the main experiment, would probably be the same as experiment_dir_in, except the experiment_dir_in can have a different path
#' @param experiment_dir_in input is the name of the main directory where the simulations will be located
#' @param params_base prefix of parameters file
#' @param job_base prefix of job key
#' @param reps repetitions for each job
#' @param n total simulations
#' 
#' @return a .sh file that is written to memory
#' @export
#' @examples
#' 
#' write_submission_array_pbs(
#'    executable_path_in = executable_path,
#'    input_files_path_in = input_files_path,
#'    experiment_supername_in = experiment_supername,
#'    experiment_name_in = experiment_name,
#'    experiment_dir_in = experiment_dir,
#'    unique_ID_vector = unique_SIM_IDs)
write_submission_array_pbs = function(experiment_supername_in,
                                  experiment_name_in,
                                  experiment_dir_in,
                                  params_base,
                                  job_base,
                                  reps, scalars, FUN, cores_in=1, walltime_in = "0:45:00",
                                  fred_home_dir_in="~/Coronavirus/FRED", fred_results_in="~/Coronavirus/FRED_RESULTS"){
    print('submit array')    
    jobname = sprintf("%s-%s",experiment_supername_in, experiment_name_in)
    tmp_cmd_file = sprintf('tmp_execute_cmd_%s.txt',jobname)
    FUN(scalars, tmp_cmd_file)    
    n = nrow(scalars)
    submission_template = "#PBS -N JOBNAME
#PBS -l nodes=1:ppn=JOBCORES
#PBS -l walltime=JOBWALLTIME
#PBS -j oe
#PBS -A PAS1694
#PBS -t 1-JOBSQUEUE

module load R/3.5.0

cd $PBS_O_WORKDIR

export FRED_HOME=FREDHOMESTR
export FRED_RESULTS=FREDRESULTSSTR
export PATH=\"${FRED_HOME}/bin:$PATH\"
##export SEMAPHORE_PARAMS=$TMPDIR/.params.lock
##export SEMAPHORE=$TMPDIR/.lock
export SEMAPHORE_PARAMS=$FRED_RESULTS/.params.lock
export SEMAPHORE=$FRED_RESULTS/.lock

file='TMPCMDFILE'
cmd=`head -n $PBS_ARRAYID $file | tail -n 1`

cd EXPERIMENTDIR

eval $cmd
"    
    submission_str = submission_template %>%
        str_replace_all(pattern="JOBNAME", replacement = jobname) %>%
        str_replace_all(pattern="EXPERIMENTDIR", replacement = experiment_dir_in) %>%
        str_replace_all(pattern="FREDHOMESTR", replacement = fred_home_dir_in) %>%
        str_replace_all(pattern="FREDRESULTSSTR", replacement = fred_results_in) %>%
        str_replace_all(pattern="JOBWALLTIME", replacement = walltime_in) %>%
        str_replace_all(pattern="JOBSQUEUE", replacement = as.character(n)) %>%
        str_replace_all(pattern="PARAMSBASE", replacement = params_base) %>%
        str_replace_all(pattern="JOBBASE", replacement = job_base) %>%
        str_replace_all(pattern="REPS", replacement = as.character(reps)) %>%
        str_replace_all(pattern="TMPCMDFILE", replacement = tmp_cmd_file) %>%
        str_replace_all(pattern="JOBCORES", replacement = as.character(cores_in))
        
    submission_file = sprintf("%s-%s.sh",experiment_supername_in,experiment_name_in)
    file.connection = file(submission_file)
    write(submission_str,file.connection)
    close(file.connection)
    return(submission_file)
}

#' Writes a submission script for a job array
#' This function takes specific parameters and creates the submission script that can be used later on to submit a job array of simulations
#' 
#' @param experiment_supername_in input the main name of the experiment, assuming there will be subexperiments for each submission
#' @param experiment_name_in input the name of the main experiment, would probably be the same as experiment_dir_in, except the experiment_dir_in can have a different path
#' @param experiment_dir_in input is the name of the main directory where the simulations will be located
#' @param params_base prefix of parameters file
#' @param job_base prefix of job key
#' @param reps repetitions for each job
#' @param n total simulations
#' 
#' @return a .sh file that is written to memory
#' @export
#' @examples
#' 
#' write_submission_array(
#'    executable_path_in = executable_path,
#'    input_files_path_in = input_files_path,
#'    experiment_supername_in = experiment_supername,
#'    experiment_name_in = experiment_name,
#'    experiment_dir_in = experiment_dir,
#'    unique_ID_vector = unique_SIM_IDs)
write_submission_array = function(experiment_supername_in,
                                  experiment_name_in,
                                  experiment_dir_in,
                                  params_base,
                                  job_base,
                                  reps, scalars, FUN, cores_in=1, walltime_in = "2:00:00",
                                  fred_home_dir_in="~/Coronavirus/FRED", fred_results_in="~/Coronavirus/FRED_RESULTS"){
    print('submit array')
    jobname = sprintf("%s-%s",experiment_supername_in, experiment_name_in)
    tmp_cmd_file = sprintf('tmp_execute_cmd_%s.txt',jobname)
    FUN(scalars, tmp_cmd_file)    
    n = nrow(scalars)
    submission_template = "#!/bin/csh
#$ -q long
#$ -t 1:JOBSQUEUE:1
#$ -N JOBNAME
#$ -pe smp JOBCORES
#$ -l h_rt=JOBWALLTIME
setenv FRED_HOME FREDHOMESTR
setenv PATH \"${FRED_HOME}/bin:$PATH\"
setenv FRED_SCR_RESULTS FREDRESULTSSTR

##setenv FRED_RESULTS /tmp/tmp_RANDOMTMP/FRED_RESULTS_${SGE_TASK_ID}
setenv FRED_RESULTS `/usr/bin/mktemp -d /tmp/tmp.XXXXXXXX`


set file='TMPCMDFILE'
set cmd=`head -n ${SGE_TASK_ID} $file | tail -n 1`

cd EXPERIMENTDIR

eval $cmd

mv ${FRED_RESULTS}/JOB/${SGE_TASK_ID} ${FRED_SCR_RESULTS}/JOB/${SGE_TASK_ID}
rm -rfv ${FRED_RESULTS}

#fred_delete -f -k JOBBASE_${SGE_TASK_ID} 
#fred_job -p PARAMSBASE_${SGE_TASK_ID}.txt -k JOBBASE_${SGE_TASK_ID} -n REPS
"    
    submission_str = submission_template %>%
        str_replace_all(pattern="JOBNAME", replacement = jobname) %>%
        str_replace_all(pattern="EXPERIMENTDIR", replacement = experiment_dir_in) %>%
        str_replace_all(pattern="FREDHOMESTR", replacement = fred_home_dir_in) %>%
        str_replace_all(pattern="FREDRESULTSSTR", replacement = fred_results_in) %>%
        str_replace_all(pattern="JOBWALLTIME", replacement = walltime_in) %>%
        str_replace_all(pattern="JOBSQUEUE", replacement = as.character(n)) %>%
        str_replace_all(pattern="PARAMSBASE", replacement = params_base) %>%
        str_replace_all(pattern="JOBBASE", replacement = job_base) %>%
        str_replace_all(pattern="REPS", replacement = as.character(reps)) %>%
        str_replace_all(pattern="TMPCMDFILE", replacement = tmp_cmd_file) %>%
        str_replace_all(pattern="JOBCORES", replacement = as.character(cores_in))
        
    submission_file = sprintf("%s-%s.sh",experiment_supername_in,experiment_name_in)
    file.connection = file(submission_file)
    write(submission_str,file.connection)
    close(file.connection)
    return(submission_file)
}


#' submits jobs to cluster
#' This function takes specific parameters and creates the submission script that can be used later on to submit a job array of simulations
#' 
#' @param experiment_supername_in input the main name of the experiment, assuming there will be subexperiments for each submission
#' @param experiment_name_in input the name of the main experiment, would probably be the same as experiment_dir_in, except the experiment_dir_in can have a different path
#' @param experiment_dir_in input is the name of the main directory where the simulations will be located
#' @param params_base prefix of parameters file
#' @param job_base prefix of job key
#' @param reps repetitions for each job
#' @param n total simulations
#' @param delete_files (TRUE or FALSE) delete files after submission
#' 
#' @return a .sh file that is written to memory
#' @export
#' @examples
#' 
#' submit_jobs(
#'    executable_path_in = executable_path,
#'    input_files_path_in = input_files_path,
#'    experiment_supername_in = experiment_supername,
#'    experiment_name_in = experiment_name,
#'    experiment_dir_in = experiment_dir,params_base,job_base,reps,n,delete_files = T)
submit_jobs = function(experiment_supername_in,
                       experiment_name_in,
                       experiment_dir_in,
                       params_base,
                       job_base,
                       reps, scalars,
                       FUN,
                       cores_in = 1,
                       delete_files = F,
                       sub_array = T,
                       subsys = "UGE", walltime_in = "0:45:00",
                       fred_home_dir_in="~/Coronavirus/FRED",
                       fred_results_in="~/Coronavirus/FRED_RESULTS"){
    if(sub_array == TRUE){
        if(subsys == "UGE"){
            submission_file = write_submission_array(
                experiment_supername_in = experiment_supername_in,
                experiment_name_in = experiment_name_in,
                experiment_dir_in = experiment_dir_in,
                params_base = params_base,
                job_base = job_base,
                reps = reps, scalars = scalars, FUN = FUN, cores_in = cores_in, walltime_in=walltime_in,
                fred_home_dir_in=fred_home_dir_in, fred_results_in=fred_results_in)
        }else if(subsys == "PBS"){
            submission_file = write_submission_array_pbs(
                experiment_supername_in = experiment_supername_in,
                experiment_name_in = experiment_name_in,
                experiment_dir_in = experiment_dir_in,
                params_base = params_base,
                job_base = job_base,
                reps = reps, scalars = scalars, FUN = FUN,
                cores_in = cores_in,walltime_in=walltime_in,
                fred_home_dir_in=fred_home_dir_in, fred_results_in=fred_results_in)
        }else if(subsys == "SLURM"){
            submission_file = write_submission_array_slurm(
                experiment_supername_in = experiment_supername_in,
                experiment_name_in = experiment_name_in,
                experiment_dir_in = experiment_dir_in,
                params_base = params_base,
                job_base = job_base,
                reps = reps, scalars = scalars,
                FUN = FUN,
                cores_in = cores_in,
                walltime_in=walltime_in,
                fred_home_dir_in=fred_home_dir_in,
                fred_results_in=fred_results_in)
        }
        if(subsys == "UGE" | subsys == "PBS"){
            system(sprintf("qsub %s", submission_file))
        }else if(subsys == "SLURM"){
            system(sprintf("sbatch %s", submission_file))
        }
        if(delete_files == TRUE){
            unlink(submission_file)
        }   
    }
    
}


#' fred_gather_data
#' 
#' Collects data from fred_jobs
#' @param params parameters of the simulation
#' @param outdir output directory
#' @param FUN function to check if simulations finished
#' @param FUN2 function to post-process data
#' @param rm_out shuold remove output directory if exists?
#' @param appendToFile (default to FALSE) should it bind rows of df or append to file?
#' @return returns 0 if things worked fine
#' 
#' @export
#' @examples
#' fred_gather_data("parameters.csv", "FRED_Sims_out")
fred_gather_data <- function(params,outdir, outfile,
                             FUN, FUN2=function(x){return(x)}, rm_out = FALSE,
                             appendToFile = FALSE,...){
    if(dir.exists(outdir) & rm_out == TRUE){
        unlink(outdir, recursive = T)
        dir.create(outdir)
    }else if (!dir.exists(outdir)){    
        dir.create(outdir)
    }
    unfinished_df = data.frame()
    outfile = file.path(outdir, outfile)
    if(file.exists(outfile)){
        unlink(outfile)
    }
    
    file.copy(params, file.path(outdir, basename(params)))
    params_orig = read_csv(params) %>%
        mutate(Finished = 0)
    fred_output = tibble()
    params_finished = tibble()
    total_finished = 0
    if(file.exists(file.path(Sys.getenv('FRED_RESULTS'),'KEY'))){
        success_read = FALSE
        nn = 1
        while(!success_read){
            if(FUN(params_orig[nn,])){
                job_processed_list = FUN2(params_orig$job_id[nn],...)
                job_processed = job_processed_list$job_df %>%
                    mutate(job_id = params_orig$job_id[nn],
                           Finished = 0)
                fred_output = job_processed[rep(1:nrow(job_processed), nrow(params_orig)),]
                success_read = TRUE
            }
            nn = nn + 1
        }
        if(nrow(fred_output) == 0){
            printf("Something went wrong with reading FRED output\n")
            stop()
        }
        output_indices = seq(from=1, by = 1, to = nrow(job_processed))
        job_rows = nrow(job_processed)
        print(sprintf("Total rows for dataframe in collected data: %d each %d rows\n", nrow(fred_output), job_rows))
        for(n in 1:nrow(params_orig)){
            if(FUN(params_orig[n,])){                
                params_orig$Finished[n] = 1
                params_tmp = params_orig[n,]
                job_processed_list = FUN2(params_orig$job_id[n],...)
                job_processed = job_processed_list$job_df %>%
                    mutate(job_id = params_orig$job_id[n],
                           Finished = 1)
                if(length(grep("extra_params_df", names(job_processed_list))) > 0){
                    params_tmp = bind_cols(params_tmp[rep(1,nrow(job_processed_list$extra_params_df)),],
                                           job_processed_list$extra_params_df)
                }
                ## Can do this faster if initialize object instead of bind_rows
                params_finished = bind_rows(params_finished, params_tmp)
                if(appendToFile == FALSE){
                    output_indx = output_indices + job_rows * (n - 1)
                    fred_output[output_indx,colnames(job_processed)] = job_processed
                }else{
                    if(total_finished == 0){
                        write_csv(job_processed, path = outfile, append = FALSE)
                    }else{
                        write_csv(job_processed, path = outfile, append = TRUE)
                    }
                }
                total_finished = total_finished + 1
            }
        }
    }    
    params_outfile = file.path(outdir, 'FRED_parameters_out.csv')
    write_csv(x=params_finished, path=params_outfile)
    if(appendToFile == FALSE){
        fred_output = fred_output %>% filter(Finished == 1)
        write_csv(fred_output, path=outfile)
    }
    return(0)
}
