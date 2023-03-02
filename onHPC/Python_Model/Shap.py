#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Feb 21 01:52:09 2023

@author: lucasliu
"""

from Ultility_Funcs.DataLoader import load_rdata
from Ultility_Funcs.TrainTest_Funcs import train_shap_regressor_model,prediction
import pandas as pd
import joblib
import os
import argparse
import shap
import re
from matplotlib import pyplot as plt

# set the font globally
plt.rcParams.update({'font.family':'sans-serif'})
# set the font name for a font family
plt.rcParams.update({'font.sans-serif':'Arial'}) #Arial
plt.rcParams['font.sans-serif']


def change_feature_name(df,new_feature_maps):
    # df = train_X2.copy()
    # new_feature_maps = feautre_name_map

    #1.Get old and new feature maps (with transofmartion features)
    old_new_dict = {}
    
    for i in range(df.shape[1]):     
        cur_col = df.columns[i]
        if 'CCS' in cur_col or 'VAL' in cur_col:
            #Get transfomation type
            if 'time_since_' in cur_col:
                new_trans_type = 'Time Since '
            elif 'time_until_' in cur_col:
                new_trans_type = 'Time Until '
            elif 'cumul_ratio_' in cur_col:
                new_trans_type = 'Cumulative occurrence of '
            else:
                new_trans_type = 'Monthly Count of'
            
            #Get Code type #DIAG or proc or drug (2ND)
            cur_feature = re.sub(r'time_since_|time_until_|cumul_ratio_', '',cur_col)
            cur_code_type = cur_feature.split('_')[1] 
            
            #Update code type and discfrip
            if cur_code_type == '2ND':
                new_code_type =  ' (Drug)'
                feature_discrip = cur_feature.replace('VAL_2ND_','')
            elif cur_code_type == 'PROC':
                new_code_type =  ' (Procedure)'
                feature_discrip = str(new_feature_maps[cur_feature]).strip()
            elif cur_code_type == 'DIAG':
                new_code_type =  ' (Diagnosis)'
                feature_discrip = str(new_feature_maps[cur_feature]).strip()
            else:
                new_code_type =  ''
                feature_discrip = ''
                
            new_name = new_trans_type + feature_discrip + new_code_type
           
            old_new_dict[cur_col] = new_name
        
        
    #Replace cumul_ratio_ as 
    df = df.rename(columns = old_new_dict)
    
    return df


def change_feature_name_importance_df(df,new_feature_maps):
    # df = importance_df.copy()
    # new_feature_maps = feautre_name_map

    #1.Get old and new feature maps (with transofmartion features)

    for i in range(df.shape[0]):     
        cur_col = df.iloc[i,0]
        if 'CCS' in cur_col or 'VAL' in cur_col:
            #Get transfomation type
            if 'time_since_' in cur_col:
                new_trans_type = 'Time Since '
            elif 'time_until_' in cur_col:
                new_trans_type = 'Time Until '
            elif 'cumul_ratio_' in cur_col:
                new_trans_type = 'Cumulative occurrence of '
            else:
                new_trans_type = 'Monthly Count of'
            
            #Get Code type #DIAG or proc or drug (2ND)
            cur_feature = re.sub(r'time_since_|time_until_|cumul_ratio_', '',cur_col)
            cur_code_type = cur_feature.split('_')[1] 
            
            #Update code type and discfrip
            if cur_code_type == '2ND':
                new_code_type =  ' (Drug)'
                feature_discrip = cur_feature.replace('VAL_2ND_','')
            elif cur_code_type == 'PROC':
                new_code_type =  ' (Procedure)'
                feature_discrip = str(new_feature_maps[cur_feature]).strip()
            elif cur_code_type == 'DIAG':
                new_code_type =  ' (Diagnosis)'
                feature_discrip = str(new_feature_maps[cur_feature]).strip()
            else:
                new_code_type =  ''
                feature_discrip = ''
                
            new_name = new_trans_type + feature_discrip + new_code_type
           
            df.iloc[i,0] = new_name
        
    return df

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
    data_dir4 = proj_dir + "20_Python_Results/" + feature_sets + "/" +  SBCE_col + "/" + model_name + "/" + "DS" + str(ds_indxes) + '/' + 'Saved_Model/' + selected_model + "/"
    data_dir5 = proj_dir + "20_Python_Results/" + feature_sets + "/" +  SBCE_col + "/" + model_name + "/" + "DS" + str(ds_indxes) + '/' + "/Prediction/" + selected_model + "/"
    data_dir6 = proj_dir + "/10G_Counts_UniqueGrp_PtsLevel/WithPossibleMonthsHasNoCodes/"

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
    
    #Load feature CCS group discrip.names, drug discrip is already in the feature name 
    ccs_diag = pd.read_excel(data_dir6 + "Count_CCS_Diag_Unique_Grps.xlsx",
                             usecols = ['Code_Grp','Grp_Discrip'])
    ccs_proc = pd.read_excel(data_dir6 + "Count_CCS_proc_Unique_Grps.xlsx",
                             usecols = ['Code_Grp','Grp_Discrip'])
    ccs_all_df = pd.concat([ccs_diag,ccs_proc], axis = 0)
    ccs_all_df['Grp_Discrip'] = ccs_all_df['Grp_Discrip'].str.replace(r'[^\w\s]+', '', regex = True)
    feautre_name_map = dict(zip(ccs_all_df['Code_Grp'], ccs_all_df['Grp_Discrip']))
    
    ccs_all_df.loc[ccs_all_df['Code_Grp'] == 'CCS_DIAG_173']
    ################################################################################
    #2.Rename feature
    ################################################################################
    train_X2 = change_feature_name(train_X2,feautre_name_map)
    test_X = change_feature_name(test_X,feautre_name_map)
    
    
    ################################################################################
    #2. Shap for all train and test
    ################################################################################ 
    #Select training set and best para
    if selected_model == "TooF_Model":
        importance_df = pd.read_csv(data_dir4 + 'importance.csv')
        importance_df = change_feature_name_importance_df(importance_df,feautre_name_map)
        top_f = list(importance_df["Feature"].iloc[0:top_f_num]) #20
        train_X = train_X2[top_f]
        train_Y = train_Y2 
    elif selected_model == "Full_Model":
        train_X = train_X2
        train_Y = train_Y2
        
    #Get optimal parameter (same for topF and  full_model)
    best_para_df = pd.read_csv(data_dir4 + 'best_para.csv', index_col = [0])
    shap_model = train_shap_regressor_model(train_X,train_Y,best_para_df,model_name)
    
    
    #Output shap model
    joblib.dump(shap_model, outdir + model_name  + '_shapmodel.pkl')
    
    
    #Shap explainer
    explainer = shap.TreeExplainer(shap_model) 
    
    
    #Test
    shap_values_test = explainer.shap_values(test_X)     
    plt.figure(figsize=(16,32))
    shap.summary_plot(shap_values_test, test_X, show=False, color_bar = True, max_display = 10)
    plt.savefig(outdir + 'SHAP_Test.png', bbox_inches='tight', dpi = 300)
        
    #Train
    shap_values_train = explainer.shap_values(train_X) 
    plt.figure(figsize=(16,32))
    shap.summary_plot(shap_values_train, train_X, show=False, color_bar = True,max_display = 10)
    plt.savefig(outdir + 'SHAP_Train.png', bbox_inches='tight', dpi = 300)

# def plot_shap_summary_trainAndTest(explainer, TRAIN_X,TEST_X,show_n,fig_size,outdir,outfile):
    
#     # TRAIN_X = train_X.iloc[0:10,:]
#     # TEST_X =  test_X.iloc[0:10,:]
#     # 
#     # outfile = "RF_SHAP_TRAIN.eps"
    
#     LARGE_SIZE =  50
#     MEDIUM_SIZE = 35
#     SMALL_SIZE = 30
#     TINY_SIZE = 20

#     #Train Values
#     shap_values_train = explainer.shap_values(TRAIN_X) #compute shap values for all X

#     #Test Values
#     shap_values_test = explainer.shap_values(TEST_X) #compute shap values for all X
    
#     plt.figure(figsize=fig_size)
#     plt.subplot(1,2,1)
#     shap.summary_plot(shap_values_train, TRAIN_X, plot_size=(fig_size[0]/2, fig_size[1]/2), show=False, color_bar = True ,max_display = show_n)
#     plt.title('Train', fontsize= MEDIUM_SIZE)
#     plt.xlabel('SHAP value',fontsize= MEDIUM_SIZE)
#     plt.yticks(fontsize= SMALL_SIZE)
#     plt.xticks(fontsize= SMALL_SIZE)

#     # ticks_value = [0,1]
#     # cb = plt.colorbar(ticks = ticks_value,aspect=1000)
#     # #cb = plt.colorbar()
#     # cb.set_ticklabels(['Low', 'High'])
#     # cb.set_label(label='Feature Value',size=SMALL_SIZE, labelpad=-50)
#     # cb.ax.tick_params(labelsize=SMALL_SIZE, length=0)
#     # cb.set_alpha(1)
#     # cb.outline.set_visible(False)
#     # bbox = cb.ax.get_window_extent().transformed(plt.gcf().dpi_scale_trans.inverted())
#     # cb.ax.set_aspect((bbox.height - 0.9) * 20)

#     #x-axis thickness
#     ax = plt.gca()
#     ax.spines['bottom'].set_linewidth(1)
        
#     plt.subplot(1,2,2)
#     shap.summary_plot(shap_values_test, TEST_X, plot_size=(fig_size[0]/2, fig_size[1]/2), show=False, color_bar = True, max_display = show_n)
#     plt.title('Test', fontsize= MEDIUM_SIZE)
#     plt.xlabel('SHAP value',fontsize= MEDIUM_SIZE)
#     plt.yticks(fontsize= SMALL_SIZE)
#     plt.xticks(fontsize= SMALL_SIZE)

#     #cb = plt.colorbar(ticks = ticks_value,aspect=1000)
#     #cb.set_ticklabels(['Low/Non-EXP.', 'High/EXP.'])
#     #cb.set_label(label='Feature Value',size=SMALL_SIZE, labelpad=-50)
#     #cb.ax.tick_params(labelsize=SMALL_SIZE, length=0)
#     #cb.set_alpha(1)
#     #cb.outline.set_visible(False)
#     #bbox = cb.ax.get_window_extent().transformed(plt.gcf().dpi_scale_trans.inverted())
#     #cb.ax.set_aspect((bbox.height - 0.9) * 20)
    
#     #x-axis thickness
#     ax = plt.gca()
#     ax.spines['bottom'].set_linewidth(1)
        
#     #space between two figure
#     plt.subplots_adjust(wspace=0.4)
    
#     plt.savefig(outdir + outfile, bbox_inches='tight', dpi = 300)
        
    
    #Shap individual:
    def plot_individual_shap(explainer, Sample_X,value_threshold, output_dir, outfile):
        # Sample_X = pd.DataFrame(test_X.iloc[sample_idx,]).transpose()
        # value_threshold = 0.1
        # outfile = sample_id
        # output_dir = outdir
        
        shap_value = explainer.shap_values(Sample_X)
        #Using logit will change log-odds numbers into probabilities, the defualt shows the the log-odds
        #We do not have to chaknge link to "logit", because we use RF_regressor classfier, the y_pred is alreayd in proabily form
        #shap.force_plot(explainer.expected_value, shap_value, Sample_X,contribution_threshold= value_threshold,show = False,matplotlib=True).savefig(output_dir + outfile,bbox_inches='tight',dpi = 500)
        plt.figure()
        shap.force_plot(explainer.expected_value, shap_value, Sample_X,contribution_threshold= value_threshold,show = False,matplotlib=True)
        plt.xticks(fontsize= 16)
        #plt.text(0.06, 0.405, plt_title, fontsize = 20)
        plt.savefig(output_dir + outfile + '.png',bbox_inches='tight',dpi = 500)
        
        plt.close()
            
    sample_id = 'ID16745@2010-11-01' #ID16745@2010-11-01 (RF), 'ID24266@2011-02-01' (XGB)
    patient_id = sample_id.split('@')[0]
    sample_idx = list(test_ID['sample_id']).index(sample_id)
    plot_individual_shap(explainer,pd.DataFrame(test_X.iloc[sample_idx,]).transpose(),
                         0.1,outdir,sample_id)

        
    #Plot Prediction trajoetory
    #Load prediction
    pred_df_m = pd.read_csv(data_dir5 + "samplelevel_prediction.csv")
    pred_df_p = pd.read_csv(data_dir5 + "patientlevel_prediction.csv")
    check = pred_df_p[['study_id','actual_label','actual_exact_monthday', 'actual_month','pred_month_th05','RAW_Month_Diff_th05']]

    sample_pred_df = pred_df_m[pred_df_m['study_id'] == patient_id]
    sample_pred_df_ptlevel = pred_df_p[pred_df_p['study_id'] == patient_id]
    sample_1st_event_df =  pts_level_char_df[pts_level_char_df['study_id'] == patient_id]
    
    #Get acutal SBCE month
    sample_acutal_month = sample_pred_df_ptlevel['actual_month'].item()
    
    #Get predict month
    sample_pred_month = sample_pred_df_ptlevel['pred_month_th05'].item()
    
    #Get breast cancer diagnosis month
    sample_primary_month = sample_1st_event_df['Date_1st_Event'].item().replace(day = 1)
    sample_primary_month_df = pd.DataFrame({'month_start':sample_primary_month,
                                            'pred_prob': pd.NA,
                                            'type': '1st_Primary_Date'}, index = [0])
    
    
    #Plot data
    plot_data = sample_pred_df[['month_start','pred_prob']].copy()
    plot_data['month_start'] = pd.to_datetime(plot_data['month_start'])
    plot_data['type'] = 'Prediction'
    plot_data = sample_primary_month_df.append(plot_data, ignore_index=True)
    
    
    event1_date = sample_primary_month
    event2_date = sample_acutal_month
    evet2_pred_date = sample_pred_month
    anote_y_pos_pred = plot_data.loc[plot_data['month_start'] == sample_pred_month,'pred_prob']
    ax = plot_data.plot('month_start','pred_prob', style = '-', color = 'steelblue', lw = 3)
    ax.set_xlabel("Time")
    ax.set_ylabel("Predicted Probability")
    ax.set_ylim(0,1)
    ax.set_xlim(min(plot_data['month_start']) - pd.DateOffset(months=3),max(plot_data['month_start']))
    ax.get_legend().remove()
    ax.annotate('1st primary \n diagnosis', xy=(event1_date, -0), xytext=(event1_date, 0.2),
                arrowprops=dict(facecolor='black', edgecolor = 'black'))
    ax.annotate('Observaed SBCE', xy=(event2_date, -0), xytext=(event2_date, 0.1),
                arrowprops=dict(facecolor='crimson', edgecolor = 'crimson'))
    ax.annotate('Predicted SBCE', xy=(evet2_pred_date, anote_y_pos_pred), xytext=(evet2_pred_date, anote_y_pos_pred + 0.2),
                arrowprops=dict(facecolor='darkcyan', edgecolor = 'darkcyan'))
    
    ax.figure.savefig(outdir + sample_id + "_Pred_Traj.png", bbox_inches='tight',dpi=200)
    
            