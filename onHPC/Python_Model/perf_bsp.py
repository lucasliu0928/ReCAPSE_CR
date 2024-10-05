#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Jul 15 23:37:25 2024

@author: lucasliu
"""

from Ultility_Funcs.DataLoader import load_rdata
from Ultility_Funcs.TrainTest_Funcs import prediction, patient_level_prediction
from Ultility_Funcs.Performance_Func import compute_performance_binary,compute_month_diff_perf
import pandas as pd
from functools import reduce
import joblib
import os
import argparse



#python3 Test.py -loc Local -fs CCSandVAL2nd -sc SBCE -mn XGB  -ds 3 -sm Full_Model -ps Grid -ts nonobv
#python3 Test.py -loc Local -fs CCSandVAL2nd -sc SBCE_Excluded_DeathPts -mn XGB  -ds 5 -sm Full_Model -ps Grid -ts nonobv
#python3 Test.py -loc Local -fs CCSandDM3SPE -sc SBCE -mn XGB  -ds 4 -sm Full_Model -ps Grid -ts nonobv
#python3 Test.py -loc Local -fs CCSandDM3SPE -sc SBCE_Excluded_DeathPts -mn XGB  -ds 6 -sm Full_Model -ps Grid -ts nonobv





if __name__ == '__main__':
    ############################################################################
    #Argments parser
    ############################################################################
    # my_parser = argparse.ArgumentParser(allow_abbrev=False)  #Construct the argument parser
    
    
    # my_parser.add_argument("-loc" , type = str , required=True, help="Data Location (e.g., 'Server', 'Local')")
    # my_parser.add_argument("-fs" , type = str ,  required=True, help="Feature set (e.g., 'CCSandVAL2nd', 'CCSandDM3SPE')")
    # my_parser.add_argument("-sc" , type = str ,  required=True, help="SBCE column (e.g., 'SBCE', 'SBCE_Excluded_DeathPts','SBCE_Excluded_DeathLabel')")
    # my_parser.add_argument("-mn" , type = str ,  required=True, help="Model name (e.g., 'RF','XGB')")
    # my_parser.add_argument("-ds" , type = int ,  required=True, help="Index of Down Sampled non-obv sample (e.g.0,1,2,...10, DS0 is the original non-obv without any ds)")
    # my_parser.add_argument("-sm" , type = str ,  required=True, help="Selected Model (e.g.TopF_Model or Full_Model)")
    # my_parser.add_argument("-ps" , type = str ,  required=True, help="Hyperparameter Search Algorithm (e.g Grid, Bayes)")
    # my_parser.add_argument("-ts" , type = str ,  required=True, help="Traning Sample data (e.g nonobv, all)")
    
    # args = vars(my_parser.parse_args())       # Parse the argument
    ####################################################################################
    #Command line input or mannual input
    ####################################################################################
    # location = args['loc']  
    # feature_sets = args['fs']
    # SBCE_col = args['sc']
    # model_name = args['mn']
    # ds_indxes = args['ds']
    # selected_model = args['sm']
    # search_alg = args['ps']
    # train_sample_type = args['ts']
    
    # #Local
    location = "Local"
    feature_sets = "CCSandDM3SPE"
    SBCE_col = "SBCE_Excluded_DeathPts" #SBCE_Excluded_DeathPts or "SBCE"
    model_name = "XGB"
    ds_indxes = 6
    selected_model = "Full_Model" # "TopF_Model or "Full_Model"
    search_alg = "Grid"
    train_sample_type = 'nonobv'
    
    if SBCE_col == "SBCE" or SBCE_col == "SBCE_Excluded_DeathPts":
      label_col   = "y_PRE_OR_POST_2ndEvent"  
    else:
      label_col   = "y_PRE_OR_POST_2ndEvent_ExcludedDeath"   
      
     
    if location == 'Server':
        proj_dir = "/users/recapse/intermediate_data/"
    elif location == 'Local':
        proj_dir = "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Intermediate_Data/0610_21/"
        
        
    #Data dir
    data_dir1 = proj_dir + "20_Python_Results/" + feature_sets + "/" +  SBCE_col + "/" + model_name + "/" + "DS" + str(ds_indxes) + '/' + train_sample_type + '/' +  search_alg + '/' + "Prediction/" + selected_model + "/"
    outdir = data_dir1 + "CI_PERF/"

    if not os.path.exists(outdir):
       # Create a new directory because it does not exist
       os.makedirs(outdir)
       print("The new directory is created!") 
    
    
    ################################################################################
    #1. Load month level Prediction
    ################################################################################       
    #Prediction month-level
    pred_df_m = pd.read_csv(data_dir1 + 'samplelevel_prediction.csv')

    #Random sample boostrap
    iteration = 1000
    perf_df_bs_list = []
    for i in range(iteration):
        if i % 100 == 0 :
            print(i)
        pred_df_sp = pred_df_m.sample(n = pred_df_m.shape[0], replace = True, random_state = i)
    
        #performance for each threshold
        thres_names = ['th0' + str(x) for x in range(1,10,1)]
        perf_df_sp_list = []
        for thres in thres_names:
            perf_df_sp = compute_performance_binary(pred_df_sp['Y_true'],
                                                   pred_df_sp['pred_prob'],
                                                   pred_df_sp['pred_class_' + thres],thres)
            perf_df_sp_list.append(perf_df_sp)
        perf_df_all_sp = pd.concat(perf_df_sp_list, axis = 0)
        perf_df_all_sp['bootstrap_sp_index'] = i
        perf_df_bs_list.append(perf_df_all_sp)
        
    perf_df_bs = pd.concat(perf_df_bs_list)
    
    #For each threshold, compute CI
    ci_perf_list = []
    for thres in thres_names:
        print(thres)
        cur_perf = perf_df_bs[perf_df_bs['Threshold']==thres]
        cur_Q975_df = cur_perf.iloc[:,0:9].quantile(0.975)
        cur_Q025_df = cur_perf.iloc[:,0:9].quantile(0.025)
        cur_mean_df = cur_perf.iloc[:,0:9].mean()
        cur_ci_perf_df = pd.concat([cur_mean_df, cur_Q025_df, cur_Q975_df], axis = 1)
        cur_ci_perf_df.rename(columns = {0.000: 'MEAN',
                                         0.025: 'Q025',
                                         0.975: 'Q975'}, inplace = True)
        cur_ci_perf_df['MEAN_CI'] = round(cur_ci_perf_df['MEAN'],2).astype(str) + '[' + round(cur_ci_perf_df['Q025'],2).astype(str) + '-' + round(cur_ci_perf_df['Q975'],2).astype(str) + ']'
        cur_ci_perf_df_t = cur_ci_perf_df.transpose()
        cur_ci_perf_df_t['Threshold'] = thres
        ci_perf_list.append(cur_ci_perf_df_t)
    ci_perf_df = pd.concat(ci_perf_list)
    ci_perf_df.to_csv(outdir + 'samplelevel_CI_performance.csv', index = True)


    ################################################################################
    #1. Load patient level Prediction
    ################################################################################       
    #Prediction month-level
    pred_df_p = pd.read_csv(data_dir1 + 'patientlevel_prediction.csv')
    

    #Random sample boostrap
    perf_df_bs_list = []
    for i in range(iteration):
        if i % 100 == 0 :
            print(i)
        pred_df_sp = pred_df_p.sample(n = pred_df_p.shape[0], replace = True, random_state = i)
    
        #performance for each threshold
        thres_names = ['th0' + str(x) for x in range(1,10,1)]
        perf_df_sp_list = []
        for thres in thres_names:
            perf_df_sp = compute_performance_binary(pred_df_sp['actual_label'],
                                                   None,
                                                   pred_df_sp['pred_label_' + thres],thres)
            month_diff_stat_df1 = compute_month_diff_perf(pred_df_sp,'ABS_Month_Diff',thres) #Abs diff
            month_diff_stat_df2 = compute_month_diff_perf(pred_df_sp,'RAW_Month_Diff',thres) #raw diff
            month_diff_stat_df = month_diff_stat_df1.merge(month_diff_stat_df2,  on = 'Num_PTS_pred_correct_label')
            perf_df_sp_comb = pd.concat([perf_df_sp,month_diff_stat_df], axis = 1)
            # move threshold to the last
            first_column = perf_df_sp_comb.pop('Threshold')
            perf_df_sp_comb.insert(perf_df_sp_comb.shape[1], 'Threshold', first_column)
            perf_df_sp_list.append(perf_df_sp_comb)
        
        perf_df_all_sp = pd.concat(perf_df_sp_list, axis = 0)
        perf_df_all_sp['bootstrap_sp_index'] = i
        perf_df_bs_list.append(perf_df_all_sp)
                    
    perf_df_bs = pd.concat(perf_df_bs_list)
    

    #For each threshold, compute CI
    ci_perf_list = []
    for thres in thres_names:
        print(thres)
        cur_perf = perf_df_bs[perf_df_bs['Threshold']==thres]
        cur_Q975_df = cur_perf.iloc[:,0:24].quantile(0.975)
        cur_Q025_df = cur_perf.iloc[:,0:24].quantile(0.025)
        cur_mean_df = cur_perf.iloc[:,0:24].mean()
        cur_ci_perf_df = pd.concat([cur_mean_df, cur_Q025_df, cur_Q975_df], axis = 1)
        cur_ci_perf_df.rename(columns = {0.000: 'MEAN',
                                         0.025: 'Q025',
                                         0.975: 'Q975'}, inplace = True)
        cur_ci_perf_df['MEAN_CI'] = round(cur_ci_perf_df['MEAN'],2).astype(str) + '[' + round(cur_ci_perf_df['Q025'],2).astype(str) + '-' + round(cur_ci_perf_df['Q975'],2).astype(str) + ']'
        cur_ci_perf_df_t = cur_ci_perf_df.transpose()
        cur_ci_perf_df_t['Threshold'] = thres
        ci_perf_list.append(cur_ci_perf_df_t)
    ci_perf_df_p = pd.concat(ci_perf_list)
    ci_perf_df_p.to_csv(outdir + 'patientlevel_CI_performance.csv', index = True)

