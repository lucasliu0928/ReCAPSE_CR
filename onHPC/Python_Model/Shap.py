#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Feb 21 01:52:09 2023

@author: lucasliu
"""

from Ultility_Funcs.DataLoader import load_rdata
from Ultility_Funcs.TrainTest_Funcs import train_shap_regressor_model
import pandas as pd
import joblib
import os
import argparse
import shap
from matplotlib import pyplot as plt

#python3 Shap.py -loc Local -fs CCSandVAL2nd -sc SBCE -mn RF  -ds 3 -sm Full_Model

if __name__ == '__main__':
    #############################################################################
    #Argments parser
    #############################################################################
    # my_parser = argparse.ArgumentParser(allow_abbrev=False)  #Construct the argument parser
    
    
    # my_parser.add_argument("-loc" , type = str , required=True, help="Data Location (e.g., 'Server', 'Local')")
    # my_parser.add_argument("-fs" , type = str ,  required=True, help="Feature set (e.g., 'CCSandVAL2nd', 'CCSandDM3SPE')")
    # my_parser.add_argument("-sc" , type = str ,  required=True, help="SBCE column (e.g., 'SBCE', 'SBCE_Excluded_DeathPts','SBCE_Excluded_DeathLabel')")
    # my_parser.add_argument("-mn" , type = str ,  required=True, help="Model name (e.g., 'RF', 'SVM','XGBoost','LR')")
    # my_parser.add_argument("-ds" , type = int ,  required=True, help="Index of Down Sampled non-obv sample (e.g.0,1,2,...10, DS0 is the original non-obv without any ds)")
    # my_parser.add_argument("-sm" , type = str ,  required=True, help="Selected Model (e.g.TopF_Model or Full_Model)")
    # my_parser.add_argument("-top_n" , type = int ,  required=True, help="Num of top ranked feature (e.g.10,20,...)")

    
    # args = vars(my_parser.parse_args())       # Parse the argument
    # #####################################################################################
    # #Command line input or mannual input
    # #####################################################################################
    # location = args['loc']  
    # feature_sets = args['fs']
    # SBCE_col = args['sc']
    # model_name = args['mn']
    # ds_indxes = args['ds']
    # selected_model = args['sm']
    # top_f_num = args['top_n']

    
    #Local
    location = "Local"
    feature_sets = "CCSandVAL2nd"
    SBCE_col = "SBCE"
    model_name = "RF"
    ds_indxes = 3
    selected_model = "Full_Model" # "TopF_Model or "Full_Model"
    top_f_num = 10
    
    if SBCE_col == "SBCE" or SBCE_col == "SBCE_Excluded_DeathPts":
      label_col   = "y_PRE_OR_POST_2ndEvent"  
    else:
      label_col   = "y_PRE_OR_POST_2ndEvent_ExcludedDeath"   
      
     
    if location == 'Server':
        proj_dir = "/users/recapse/intermediate_data/"
    elif location == 'Local':
        proj_dir = "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Intermediate_Data/0610_21/"
        
        
    #Data dir
    data_dir1 = proj_dir + "15_XGB_Input/" + feature_sets + "/All_Samples/" + SBCE_col + "/Train/"
    data_dir2 = proj_dir +  "15_XGB_Input/" + feature_sets + "/All_Samples/" + SBCE_col + "/Test/"
    data_dir3 = proj_dir +  "8_Characteristics2/Patient_Level/"
    data_dir4 = proj_dir + "20_Python_Results/" + feature_sets + "/" +  SBCE_col + "/" + model_name + "/" + "DS" + str(ds_indxes) + '/' + 'Saved_Model/Full_Model/'
    outdir = proj_dir + "20_Python_Results/" + feature_sets + "/" +  SBCE_col + "/" + model_name + "/" + "DS" + str(ds_indxes) + '/'
    outdir = outdir + "Shap/" + selected_model + "/"


    if not os.path.exists(outdir):
       # Create a new directory because it does not exist
       os.makedirs(outdir)
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
    
    #Load test data
    test_X1, test_Y1, test_ID1 =load_rdata(data_dir2,'test_neg_data.rda','test_neg_df',label_col)
    test_X2, test_Y2, test_ID2 =load_rdata(data_dir2,'test_nonobv_data.rda','test_nonobv_df',label_col)
    test_X3, test_Y3, test_ID3 =load_rdata(data_dir2,'test_pos_data.rda','test_pos_df',label_col)
    test_X = pd.concat([test_X1,test_X2,test_X3], axis = 0)
    test_X.reset_index(inplace = True, drop = True)
    test_Y = pd.concat([test_Y1,test_Y2,test_Y3], axis = 0)
    test_Y.reset_index(inplace = True, drop = True)
    test_ID = pd.concat([test_ID1,test_ID2,test_ID3], axis = 0)
    test_ID.reset_index(inplace = True,drop = True)
    
    #Load SBCE month label patient -level
    pts_level_char_df = pd.read_excel(data_dir3 + "8_PatientLevel_char_WithPossibleMonthsHasNoCodes.xlsx",
                                      usecols = ['study_id','SBCE','SBCE_Excluded_DeathLabel','Date_2nd_Event','Date_1st_Event'],
                                      parse_dates= ['Date_2nd_Event','Date_1st_Event'],
                                      sheet_name = 0)
    pts_level_char_df['study_id'] = 'ID' + pts_level_char_df['study_id'].astype(str)
    
    ################################################################################
    #2. Shap for all train and test
    ################################################################################ 
    #Select training set and best para
    if selected_model == "TooF_Model":
        importance_df = pd.read_csv(data_dir4 + 'importance.csv')
        top_f = list(importance_df["Feature"].iloc[0:top_f_num]) #20
        train_X = train_X2[top_f]
        train_Y = train_Y2 
    elif selected_model == "Full_Model":
        train_X = train_X2
        train_Y = train_Y2
        
    #Get optimal parameter (same for topF and  full_model)
    best_para_df = pd.read_csv(data_dir4 + 'best_para.csv')
    shap_model = train_shap_regressor_model(train_X,train_Y,best_para_df,'RF')
    
    
    #Output shap model
    joblib.dump(shap_model, outdir + model_name +'_TopFeature_model.pkl')
    
    
    #Shap explainer
    explainer = shap.TreeExplainer(shap_model) 
    
    
    #Test
    shap_values_test = explainer.shap_values(test_X.iloc[0:10,:]) 
    shap.summary_plot(shap_values_test, test_X, show=True, color_bar = True, max_display = 10)
    
    
        
    # #Train
    # shap_values_train = explainer.shap_values(train_X) 
    # shap.summary_plot(shap_values_train, train_X, show=False, color_bar = True,max_display = 10)


        
    
    # #Shap individual:
    # sample_id = 'ID16745@2010-11-01'
    # sample_idx = list(test_ID['sample_id']).index(sample_id)
        
    # def plot_individual_shap(explainer, Sample_X,value_threshold, output_dir, outfile):
    #     Sample_X = test_X.iloc[sample_idx]
    #     value_threshold = 0.1
    #     outfile = sample_id
    #     output_dir = outdir
        
    #     shap_value = explainer.shap_values(Sample_X)
    #     #Using logit will change log-odds numbers into probabilities, the defualt shows the the log-odds
    #     #We do not have to chaknge link to "logit", because we use RF_regressor classfier, the y_pred is alreayd in proabily form
    #     #shap.force_plot(explainer.expected_value, shap_value, Sample_X,contribution_threshold= value_threshold,show = False,matplotlib=True).savefig(output_dir + outfile,bbox_inches='tight',dpi = 500)
    #     plt.figure()
    #     shap.force_plot(explainer.expected_value, shap_value, Sample_X,contribution_threshold= value_threshold,show = False,matplotlib=True)
    #     plt.xticks(fontsize= 16)
    #     #plt.text(0.06, 0.405, plt_title, fontsize = 20)
    #     plt.savefig(output_dir + outfile + '.png',bbox_inches='tight',dpi = 500)
        
    #     plt.close()
        
    # #Plot Prediction trajoetory
    # sample_pred_df = pred_df_monthlevel[pred_df_monthlevel['study_id'] == sample_id.split('@')[0]]
    # sample_pred_df_ptlevel = pred_df_ptlevel[pred_df_ptlevel['study_id'] == sample_id.split('@')[0]]
    # sample_1st_event_df =  pts_level_char_df[pts_level_char_df['study_id'] == sample_id.split('@')[0]]
    
    # #Get acutal SBCE month
    # sample_acutal_month = sample_pred_df_ptlevel['actual_month'].item()
    
    # #Get predict month
    # sample_pred_month = sample_pred_df_ptlevel['pred_month'].item()
    
    # #Get breast cancer diagnosis month
    # sample_primary_month = sample_1st_event_df['Date_1st_Event'].item().replace(day = 1)
    # sample_primary_month_df = pd.DataFrame({'sample_id_split1':sample_primary_month,
    #                                         'pred_prob': pd.NA,
    #                                         'type': '1st_Primary_Date'}, index = [0])
    
    
    # #Plot data
    # plot_data = sample_pred_df[['sample_id_split1','pred_prob']].copy()
    # plot_data['sample_id_split1'] = pd.to_datetime(plot_data['sample_id_split1'])
    # plot_data['type'] = 'Prediction'
    # plot_data = sample_primary_month_df.append(plot_data, ignore_index=True)
    
    
    # event1_date = sample_primary_month
    # event2_date = sample_acutal_month
    # evet2_pred_date = sample_pred_month
    # anote_y_pos_pred = plot_data.loc[plot_data['sample_id_split1'] == sample_pred_month,'pred_prob']
    # ax = plot_data.plot('sample_id_split1','pred_prob', style = '-', color = 'steelblue', lw = 3)
    # ax.set_xlabel("Time")
    # ax.set_ylabel("Predicted Probability")
    # ax.set_ylim(0,1)
    # ax.set_xlim(min(plot_data['sample_id_split1']) - pd.DateOffset(months=3),max(plot_data['sample_id_split1']))
    # ax.get_legend().remove()
    # ax.annotate('1st primary \n diagnosis', xy=(event1_date, -0), xytext=(event1_date, 0.1),
    #             arrowprops=dict(facecolor='black', edgecolor = 'black'))
    # ax.annotate('Observaed SBCE', xy=(event2_date, -0), xytext=(event2_date, 0.1),
    #             arrowprops=dict(facecolor='crimson', edgecolor = 'crimson'))
    # ax.annotate('Predicted SBCE', xy=(evet2_pred_date, anote_y_pos_pred), xytext=(evet2_pred_date, anote_y_pos_pred + 0.2),
    #             arrowprops=dict(facecolor='darkcyan', edgecolor = 'darkcyan'))
    
    # ax.figure.savefig(outdir + sample_id + "_Pred_Traj.png", bbox_inches='tight',dpi=200)
    
            