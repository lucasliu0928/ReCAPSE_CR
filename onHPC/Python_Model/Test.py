#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Feb 20 21:58:45 2023

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



#python3 Test.py -loc Local -fs CCSandVAL2nd -sc SBCE -mn XGB  -ds 3 -sm Full_Model -ps Grid
#python3 Test.py -loc Local -fs CCSandVAL2nd -sc SBCE_Excluded_DeathPts -mn XGB  -ds 5 -sm Full_Model -ps Grid
#python3 Test.py -loc Local -fs CCSandDM3SPE -sc SBCE -mn XGB  -ds 4 -sm Full_Model -ps Grid
#python3 Test.py -loc Local -fs CCSandDM3SPE -sc SBCE_Excluded_DeathPts -mn XGB  -ds 6 -sm Full_Model -ps Grid




if __name__ == '__main__':
    ############################################################################
    #Argments parser
    ############################################################################
    my_parser = argparse.ArgumentParser(allow_abbrev=False)  #Construct the argument parser
    
    
    my_parser.add_argument("-loc" , type = str , required=True, help="Data Location (e.g., 'Server', 'Local')")
    my_parser.add_argument("-fs" , type = str ,  required=True, help="Feature set (e.g., 'CCSandVAL2nd', 'CCSandDM3SPE')")
    my_parser.add_argument("-sc" , type = str ,  required=True, help="SBCE column (e.g., 'SBCE', 'SBCE_Excluded_DeathPts','SBCE_Excluded_DeathLabel')")
    my_parser.add_argument("-mn" , type = str ,  required=True, help="Model name (e.g., 'RF','XGB')")
    my_parser.add_argument("-ds" , type = int ,  required=True, help="Index of Down Sampled non-obv sample (e.g.0,1,2,...10, DS0 is the original non-obv without any ds)")
    my_parser.add_argument("-sm" , type = str ,  required=True, help="Selected Model (e.g.TopF_Model or Full_Model)")
    my_parser.add_argument("-ps" , type = str ,  required=True, help="Hyperparameter Search Algorithm (e.g Grid, Bayes)")

    
    args = vars(my_parser.parse_args())       # Parse the argument
    ####################################################################################
    #Command line input or mannual input
    ####################################################################################
    location = args['loc']  
    feature_sets = args['fs']
    SBCE_col = args['sc']
    model_name = args['mn']
    ds_indxes = args['ds']
    selected_model = args['sm']
    search_alg = args['ps']
    
    # #Local
    # location = "Local"
    # feature_sets = "CCSandVAL2nd"
    # SBCE_col = "SBCE_Excluded_DeathPts"
    # model_name = "XGB"
    # ds_indxes = 5
    # selected_model = "TopF_Model" # "TopF_Model or "Full_Model"
    # search_alg = "Grid"
    
    if SBCE_col == "SBCE" or SBCE_col == "SBCE_Excluded_DeathPts":
      label_col   = "y_PRE_OR_POST_2ndEvent"  
    else:
      label_col   = "y_PRE_OR_POST_2ndEvent_ExcludedDeath"   
      
     
    if location == 'Server':
        proj_dir = "/users/recapse/intermediate_data/"
    elif location == 'Local':
        proj_dir = "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Intermediate_Data/0610_21/"
        
        
    #Data dir
    data_dir1 = proj_dir +  "15_XGB_Input/" + feature_sets + "/All_Samples/" + SBCE_col + "/Test/"
    data_dir2 = proj_dir +  "8_Characteristics2/Patient_Level/"
    data_dir3 = proj_dir + "20_Python_Results/" + feature_sets + "/" +  SBCE_col + "/" + model_name + "/" + "DS" + str(ds_indxes) + '/' + search_alg + '/'
    data_dir4 = data_dir3 + 'Saved_Model/' + selected_model + "/"


    outdir = data_dir3 + "Prediction/" + selected_model + "/"


    if not os.path.exists(outdir):
       # Create a new directory because it does not exist
       os.makedirs(outdir)
       print("The new directory is created!") 
    
    
    ####################################################################################################
    #1. Load data 
    ####################################################################################################
    #Load test data
    test_X1, test_Y1, test_ID1 =load_rdata(data_dir1,'test_neg_data.rda','test_neg_df',label_col)
    test_X2, test_Y2, test_ID2 =load_rdata(data_dir1,'test_nonobv_data.rda','test_nonobv_df',label_col)
    test_X3, test_Y3, test_ID3 =load_rdata(data_dir1,'test_pos_data.rda','test_pos_df',label_col)
    test_X = pd.concat([test_X1,test_X2,test_X3], axis = 0)
    test_X.reset_index(inplace = True, drop = True)
    test_Y = pd.concat([test_Y1,test_Y2,test_Y3], axis = 0)
    test_Y.reset_index(inplace = True, drop = True)
    test_ID = pd.concat([test_ID1,test_ID2,test_ID3], axis = 0)
    test_ID.reset_index(inplace = True,drop = True)
    
    #Load SBCE month label patient -level
    pts_level_char_df = pd.read_excel(data_dir2 + "8_PatientLevel_char_WithPossibleMonthsHasNoCodes.xlsx",
                                      usecols = ['study_id','SBCE','SBCE_Excluded_DeathLabel','Date_2nd_Event','Date_1st_Event'],
                                      parse_dates= ['Date_2nd_Event','Date_1st_Event'],
                                      sheet_name = 0)
    pts_level_char_df['study_id'] = 'ID' + pts_level_char_df['study_id'].astype(str)
    
    ################################################################################
    #2. Prediction
    ################################################################################       
    #Load model 
    if selected_model == "TopF_Model":
        trained_model = joblib.load(data_dir4 + model_name + "_TopFeature_model.pkl") #topFmodel
        import_features_df = pd.read_csv(data_dir4 + "importance.csv")
        import_features  = list(import_features_df['Feature'])
        test_X = test_X[import_features]
    elif selected_model == "Full_Model":
        trained_model = joblib.load(data_dir4 + model_name + "_Fullmodel.pkl") #Fullmodel

    #Prediction month-level
    pred_df_m = prediction(trained_model,test_X,test_Y,test_ID)
    pred_df_m.to_csv(outdir + 'samplelevel_prediction.csv', index = False)


    #performance month-level 
    thres_names = ['th0' + str(x) for x in range(1,10,1)]
    perf_df_m_list = []
    for thres in thres_names:
        perf_df_m = compute_performance_binary(pred_df_m['Y_true'],
                                               pred_df_m['pred_prob'],
                                               pred_df_m['pred_class_' + thres],thres)
        perf_df_m_list.append(perf_df_m)
    perf_df_all_m = pd.concat(perf_df_m_list, axis = 0)
    perf_df_all_m.to_csv(outdir + 'samplelevel_performance.csv', index = False)

    #Prediction Patient-level (3month consecutive method)
    thres_list = [0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9]
    pred_df_p_list = []
    for thres in thres_list :
        pred_df_p  = patient_level_prediction(pred_df_m,thres,pts_level_char_df,"SBCE",pred_method="3month")
        sufix_col =  str(thres).replace('.','')
        pred_df_p.rename(columns = {'pred_label': 'pred_label_th' + sufix_col,
                                    'pred_month': 'pred_month_th' + sufix_col,
                                    'RAW_Month_Diff': 'RAW_Month_Diff_th' + sufix_col,
                                    'ABS_Month_Diff': 'ABS_Month_Diff_th' + sufix_col},
                         inplace = True)
        pred_df_p_list.append(pred_df_p)
        
    
    pred_df_p_all = reduce(lambda x, y: pd.merge(x, y, on = ['study_id',
                                                             'actual_label',
                                                             'actual_exact_monthday',
                                                             'actual_month']), pred_df_p_list)
    pred_df_p_all.to_csv(outdir + 'patientlevel_prediction.csv', index = False)


    #perforamnce patient-level
    thres_names = ['th0' + str(x) for x in range(1,10,1)]
    perf_df_p_list = []
    for thres in thres_names:
        perf_df_p = compute_performance_binary(pred_df_p_all['actual_label'],
                                               None,
                                               pred_df_p_all['pred_label_' + thres],thres)
        month_diff_stat_df1 = compute_month_diff_perf(pred_df_p_all,'ABS_Month_Diff',thres) #Abs diff
        month_diff_stat_df2 = compute_month_diff_perf(pred_df_p_all,'RAW_Month_Diff',thres) #raw diff
        month_diff_stat_df = month_diff_stat_df1.merge(month_diff_stat_df2,  on = 'Num_PTS_pred_correct_label')
        perf_df_comb = pd.concat([perf_df_p,month_diff_stat_df], axis = 1)
        perf_df_p_list.append(perf_df_comb)
        
    perf_df_all_p = pd.concat(perf_df_p_list, axis = 0)
    
    # move threshold to the begining
    first_column = perf_df_all_p.pop('Threshold')
    perf_df_all_p.insert(0, 'Threshold', first_column)

    perf_df_all_p.to_csv(outdir + 'patientlevel_performance.csv', index = False)

