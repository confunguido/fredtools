#!/bin/csh
#$ -M gcamargo@nd.edu
#$ -m a
#$ -q long
#$ -N FRED_nursing

setenv FRED_HOME ~/Coronavirus/FRED
setenv PATH "${FRED_HOME}/bin:$PATH"
setenv FRED_RESULTS ~/Coronavirus/FRED_RESULTS

#rm -rf  ${FRED_RESULTS}
#mkdir ${FRED_RESULTS}

cd scripts
fred_job -p ../input_files/params_covid.txt -k chicago_nursing -n 1
