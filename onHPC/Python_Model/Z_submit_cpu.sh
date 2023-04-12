#!/bin/bash

# V.Gazula 1/8/2019

#SBATCH -t 800:00:00                             #Time for the job to run
#SBATCH --job-name=GPISBCE                        #Name of the job
#SBATCH -N 1                                    #Number of nodes required
#SBATCH -n 1
#SBATCH -c 30                            #Number of cores needed for the job
#SBATCH --mem=120g                       # 32 GB ram asked
#SBATCH --nodelist="hpc-cmp4" 				 #Name of the queue hpc-2
#SBATCH --partition=main                          
#SBATCH --mail-type ALL                         #Send email on start/end
#SBATCH --mail-user jli394@uky.edu               #Where to send email



#python Train.py -loc Server -fs CCSandVAL2nd -sc SBCE -mn XGB -top_n 10 -ds 3 -ps Bayes
#python3 Train.py -loc Server -fs CCSandVAL2nd -sc SBCE_Excluded_DeathPts -mn XGB -top_n 10 -ds 5 -ps Bayes
#python3 Train.py -loc Server -fs CCSandDM3SPE -sc SBCE -mn XGB -top_n 10 -ds 4 -ps Bayes
#python3 Train.py -loc Server -fs CCSandDM3SPE -sc SBCE_Excluded_DeathPts -mn XGB -top_n 10 -ds 6 -ps Bayes

python3 Test.py -loc Server -fs CCSandVAL2nd -sc SBCE -mn XGB  -ds 3 -sm Full_Model -ps Bayes
#python3 Test.py -loc Server -fs CCSandVAL2nd -sc SBCE_Excluded_DeathPts -mn XGB  -ds 5 -sm Full_Model -ps Bayes
#python3 Test.py -loc Server -fs CCSandDM3SPE -sc SBCE -mn XGB  -ds 4 -sm Full_Model -ps Bayes
#python3 Test.py -loc Server -fs CCSandDM3SPE -sc SBCE_Excluded_DeathPts -mn XGB  -ds 6 -sm Full_Model -ps Bayes