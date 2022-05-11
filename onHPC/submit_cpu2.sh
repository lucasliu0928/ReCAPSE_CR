#!/bin/bash

# V.Gazula 1/8/2019

#SBATCH -t 800:00:00                             #Time for the job to run
#SBATCH --job-name=topfmodel                        #Name of the job
#SBATCH -N 1                                    #Number of nodes required
#SBATCH -n 1
#SBATCH -c 30                            #Number of cores needed for the job
#SBATCH --mem=120g                       # 32 GB ram asked
#SBATCH --nodelist="hpc-3" 				 #Name of the queue hpc-2
#SBATCH --partition=debug                          
#SBATCH --mail-type ALL                         #Send email on start/end
#SBATCH --mail-user jli394@uky.edu               #Where to send email


#/usr/bin/Rscript --vanilla 16C_Prediction_TrainData.R
/usr/bin/Rscript --vanilla 16D_Prediction_TestData.R

#python transfer_input-1.py
