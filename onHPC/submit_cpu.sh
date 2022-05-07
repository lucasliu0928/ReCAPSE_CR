#!/bin/bash

# V.Gazula 1/8/2019

#SBATCH -t 800:00:00                             #Time for the job to run
#SBATCH --job-name=val2nd                        #Name of the job
#SBATCH -N 1                                    #Number of nodes required
#SBATCH -n 1
#SBATCH -c 30                            #Number of cores needed for the job
#SBATCH --mem=120g                    # 32 GB ram asked
#SBATCH --partition="hpc-4"                           #Name of the queue hpc-2
#SBATCH --mail-type ALL                         #Send email on start/end
#SBATCH --mail-user jli394@uky.edu               #Where to send email



/usr/bin/Rscript --vanilla 10E_Get_PerMonth_DM3GEN_AllEnrolls.R
#Rscript --vanilla step2_check_column.R

#python transfer_input-1.py
