#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Feb 20 19:39:50 2023

@author: lucasliu
@NOTE: Best perforamnce ds index 
For CCSandVAL2nd:
ds_index = 3 for SBCE
ds_index = 5 for SBCE_Excluded_DeathPts

For CCSandDM3SPE:
ds_index = 4 for SBCE
ds_index = 6 for SBCE_Excluded_DeathPts
"""

from Ultility_Funcs.DataLoader import load_rdata
from Ultility_Funcs.TrainTest_Funcs import train_model_cv_gridsearch,get_default_importance_feature,train_topf_model
import joblib
import os
import argparse


#python3 Train.py -loc Local -fs CCSandVAL2nd -sc SBCE_Excluded_DeathPts -mn XGB -top_n 10 -ds 5 -ps Grid

if __name__ == '__main__':
    #############################################################################
    #Argments parser
    #############################################################################
    my_parser = argparse.ArgumentParser(allow_abbrev=False)  #Construct the argument parser
    
    
    my_parser.add_argument("-loc" , type = str , required=True, help="Data Location (e.g., 'Server', 'Local')")
    my_parser.add_argument("-fs" , type = str ,  required=True, help="Feature set (e.g., 'CCSandVAL2nd', 'CCSandDM3SPE')")
    my_parser.add_argument("-sc" , type = str ,  required=True, help="SBCE column (e.g., 'SBCE', 'SBCE_Excluded_DeathPts','SBCE_Excluded_DeathLabel')")
    my_parser.add_argument("-mn" , type = str ,  required=True, help="Model name (e.g., 'RF','XGB')")
    my_parser.add_argument("-top_n" , type = int ,  required=True, help="Num of top ranked feature (e.g.10,20,...)")
    my_parser.add_argument("-ds" , type = int ,  required=True, help="Index of Down Sampled non-obv sample (e.g.0,1,2,...10, DS0 is the original non-obv without any ds)")
    my_parser.add_argument("-ps" , type = str ,  required=True, help="Hyperparameter Search Algorithm (e.g Grid, Bayes)")

    
    args = vars(my_parser.parse_args())       # Parse the argument
    #####################################################################################
    #Command line input or mannual input
    #####################################################################################
    location = args['loc']  
    feature_sets = args['fs']
    SBCE_col = args['sc']
    model_name = args['mn']
    top_f_num = args['top_n']
    ds_indxes = args['ds']
    search_alg = args['ps']
    
    # #Local
    # location = "Local"
    # feature_sets = "CCSandVAL2nd"
    # SBCE_col = "SBCE_Excluded_DeathPts"
    # model_name = "XGB"
    # top_f_num = 10
    # ds_indxes = 5
    # search_alg = "Grid"
    
    if SBCE_col == "SBCE" or SBCE_col == "SBCE_Excluded_DeathPts":
      label_col   = "y_PRE_OR_POST_2ndEvent"  
    else:
      label_col   = "y_PRE_OR_POST_2ndEvent_ExcludedDeath"   #this treat death pts as label 0, only for SBCE_Exclude_Deathlabel choice
      
     
    if location == 'Server':
        proj_dir = "/users/recapse/intermediate_data/"
    elif location == 'Local':
        proj_dir = "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Intermediate_Data/0610_21/"
        


        
        
    #Data dir
    data_dir1 = proj_dir + "15_XGB_Input/" + feature_sets + "/All_Samples/" + SBCE_col + "/Train/"
    outdir = proj_dir + "20_Python_Results/" + feature_sets + "/" +  SBCE_col + "/" + model_name + "/" + "DS" + str(ds_indxes) + '/' + search_alg + '/'
    outdir1 = outdir + "Saved_Model/Full_Model/"
    outdir2 = outdir + "Saved_Model/TopF_Model/"

    for odir in [outdir1,outdir2]:
        if not os.path.exists(odir):
           # Create a new directory because it does not exist
           os.makedirs(odir)
           print("The new directory is created!") 
    
    
    ####################################################################################################
    #1. Load data 
    ####################################################################################################    
    #Load train data
    #train_X1, train_Y1, _ =load_rdata(data_dir1,'train_neg_data.rda','train_neg_df',label_col)
    train_X2, train_Y2, _ =load_rdata(data_dir1,'train_nonobv_DS'+ str(ds_indxes) + '.rda','train_nonobv_ds_df',label_col)
    #train_X3, train_Y3, _ =load_rdata(data_dir1,'train_pos_data.rda','train_pos_df',label_col)
    # train_X_All = pd.concat([train_X1,train_X2,train_X3], axis = 0)
    # train_Y_All = pd.concat([train_Y1,train_Y2,train_Y3], axis = 0)
    # train_comb = pd.concat([train_X_All,train_Y_All], axis = 1)
    
    ################################################################################
    #Train Full RF Model using nonObv sample
    #using CV with grid search to get optimal model
    ################################################################################
    #Train nonobv
    train_X = train_X2
    train_Y = train_Y2
    
    if search_alg == 'Grid':
        optimal_model, best_para_df = train_model_cv_gridsearch(train_X,train_Y, model_name)
    else:
        pass
    
    #Output optimal model
    joblib.dump(optimal_model, outdir1 + model_name + '_Fullmodel.pkl')
    best_para_df.to_csv(outdir1 + 'best_para.csv')
    
    #get importance 
    feature_names = train_X.columns
    importance_df = get_default_importance_feature(optimal_model,feature_names,model_name)
    importance_df.to_csv(outdir1 + 'importance.csv', index = False)
    
    
    ################################################################################
    #Train nonobv top X feature
    ################################################################################
    top_f = list(importance_df["Feature"].iloc[0:top_f_num]) #20
    train_X = train_X[top_f]
    train_Y = train_Y 
    topf_model = train_topf_model(train_X, train_Y, best_para_df, model_name)

    #Output optimal model
    joblib.dump(topf_model,  outdir2 + model_name +'_TopFeature_model.pkl')
    
    
    #get importance 
    feature_names = train_X.columns
    importance_df = get_default_importance_feature(topf_model,feature_names,model_name)
    importance_df.to_csv(outdir2 + 'importance.csv', index = False)
