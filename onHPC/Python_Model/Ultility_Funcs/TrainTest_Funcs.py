#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Feb 20 20:03:35 2023

@author: lucasliu
"""


from sklearn.ensemble import RandomForestClassifier,RandomForestRegressor
from sklearn.model_selection import GridSearchCV
from skopt import BayesSearchCV
import pandas as pd
import numpy as np
import xgboost as xgb


def downsample_func(in_data,out_col,ran_state):
    # data of each class
    in_class0 = in_data[in_data[out_col] == 0]
    in_class1 = in_data[in_data[out_col] == 1]

    #num of class 1
    n_class1 = in_class1.shape[0]
    
    #random sample num = n_class1 of class 0
    in_class0_downsampled = in_class0.sample(n= n_class1, replace=False, random_state=ran_state)
    
    #down sampled data
    in_data_downsampled = pd.concat([in_class1, in_class0_downsampled], axis = 0)
    
    return in_data_downsampled


def train_model_cvsearch(train_data, train_label, model_name,opt_alg = "Grid"):
    r'''
    This function use CV to get optimal model
    '''
    #Instantiation of the model
    if model_name == 'RF':
        model = RandomForestClassifier(random_state = 0)
        param_grid = {'n_estimators': [200, 500],
                      'max_depth' : [6,7,8]}
    elif model_name == 'XGB':
        model = xgb.XGBClassifier(objective="binary:logistic", 
                                  eval_metric="auc",
                                  use_label_encoder=False,
                                  random_state=0)
        param_grid = {'max_depth': [6,7,8],
                      'max_leaves': [0,1,2]}
        


    #CV 
    if opt_alg == 'Grid':
        cv_model = GridSearchCV(model, param_grid, cv = 10)
    elif opt_alg == "Bayes":
        cv_model = BayesSearchCV(model, param_grid, cv = 10)

    cv_model.fit(train_data, train_label)
    
    #Get best parameters
    best_para = cv_model.best_params_ 
    best_para_df = pd.DataFrame.from_dict(best_para, orient = 'index', columns = ['value'])
    
    #Re-train with optiaml model
    if model_name == 'RF':
        model = RandomForestClassifier(max_depth= best_para['max_depth'], 
                                          n_estimators= best_para['n_estimators'],
                                          random_state=0)
    elif model_name == 'XGB':
        model = xgb.XGBClassifier(max_depth = best_para['max_depth'],
                                  max_leaves = best_para['max_leaves'],
                                  objective="binary:logistic", 
                                  eval_metric="auc",
                                  use_label_encoder=False,
                                  random_state=0)
        
    model.fit(train_data, train_label)
    
    
    return model,best_para_df

def get_default_importance_feature(trained_model,feature_names, model_name):        
    if model_name == "RF":
        feature_import = trained_model.feature_importances_
    else:
        feature_import = trained_model.feature_importances_
    
    important_df = pd.DataFrame({"Feature" :feature_names, "importances": feature_import})
    important_df.sort_values(by = "importances", inplace = True, ascending = False)

    return important_df
    
def train_topf_model(train_data, train_label, parameters_df, model_name):
    #Train 
    if model_name == 'RF':
        topf_model = RandomForestClassifier(max_depth= parameters_df.loc['max_depth','value'], 
                                          n_estimators= parameters_df.loc['n_estimators','value'],
                                          random_state=0)
    elif model_name == 'XGB':
        topf_model = xgb.XGBClassifier(max_depth = parameters_df.loc['max_depth','value'],
                                       max_leaves = parameters_df.loc['max_leaves','value'],
                                       objective="binary:logistic", 
                                       eval_metric="auc",
                                       use_label_encoder=False,
                                       random_state=0)
    
    topf_model.fit(train_data, train_label)
        
    return topf_model


def train_shap_regressor_model(train_data, train_label, parameters_df, model_name):
    #Train 
    if model_name == 'RF':
        shap_model = RandomForestRegressor(max_depth= parameters_df.loc['max_depth','value'], 
                                          n_estimators= parameters_df.loc['n_estimators','value'],
                                          random_state=0)
    elif model_name == 'XGB':
        shap_model = xgb.XGBRegressor(max_depth = parameters_df.loc['max_depth','value'],
                                       max_leaves = parameters_df.loc['max_leaves','value'],
                                       random_state=0)
    
    shap_model.fit(train_data, train_label)
        
    return shap_model
    
    
def prediction(model,test_data,test_label,ID,method = "Classifier"):
    #Prediction prob
    if method == "Classifier":
        pred_prob = model.predict_proba(test_data)[: ,1] #probabily of prediction class label 1
    elif method == "Regressor":
        pred_prob = model.predict(test_data)
    pred_prob_df = pd.DataFrame({'Y_true':    test_label, 
                                 'pred_prob': pred_prob})
        
    #Prediction class
    thres_list = [0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9]
    pred_classes_list = []
    for thres in thres_list:
        pred_classes = (pred_prob > thres).astype(int)
        pred_classes = pd.DataFrame({'pred_class_th' + str(thres).replace('.',''): pred_classes})
        pred_classes_list.append(pred_classes)
    pred_classes_df = pd.concat(pred_classes_list, axis = 1)

    #Store in one table
    pred_table = pd.concat([ID, pred_prob_df,pred_classes_df], axis = 1)
        
    #Split sample Id and month
    id_month_df = pred_table['sample_id'].str.split('@',expand=True).add_prefix('sample_id_split')
    id_month_df.columns = ['study_id','month_start']
    pred_table = pd.concat([id_month_df,pred_table.iloc[:,1:]], axis = 1) 
    
    
    return pred_table


def reformat_month_col_one_pt(indata):
    indata['month_start'] = pd.to_datetime(indata['month_start'])
    
    #Sort by date and time
    indata.sort_values(by = ['month_start'], inplace = True)
    
    #get month index (current month - the first month of each patient)
    indata['month_index'] = round(((indata['month_start'] -  indata['month_start'].iloc[0])/np.timedelta64(1, 'M'))).astype(int)
    
    return indata

def prediction_sbce_month_consecutiveMethod(indata, pred_threshold):    
    r'''
    This function predict sbce month (the first month of 3 consecutive month  > threshold) 

    '''
    cond = indata['pred_prob'] > pred_threshold
    candidate_month = list(indata.loc[cond, 'month_index'])
    candidate_month = sorted(candidate_month)
    
    if len(candidate_month) > 3:#at least 3 month > threshold

        for i in range(len(candidate_month) - 2):
         
            # check if consectivie month exsit
            if candidate_month[i] == candidate_month[i + 1] - 1 and candidate_month[i+1] == candidate_month[i + 2] - 1:
         
                pred_month_idx = candidate_month[i]
                break
            else:
                pred_month_idx = None
                
        if pred_month_idx is not None:
            cond = indata['month_index'] == pred_month_idx
            pred_month = indata.loc[cond,'month_start'].item()
        else:
            pred_month = None
            
    else:
        pred_month = None
            
    return pred_month


def prediction_sbce_month_1stMonthMethod(indata, pred_threshold):    
    r'''
    This function predict sbce month (the first month  > threshold) 

    '''
    cond = indata['pred_prob'] > pred_threshold
    candidate_month = list(indata.loc[cond, 'month_index'])
    candidate_month = sorted(candidate_month)
    
    if len(candidate_month) > 1:#at least 1 month > threshold
    
        pred_month_idx = candidate_month[0]
        cond = indata['month_index'] == pred_month_idx
        pred_month = indata.loc[cond,'month_start'].item()

            
    else:
        pred_month = None
            
    return pred_month



def patient_level_prediction(prediction_df,threshold,label_df,SBCE_Column, pred_method = "3month"):
    #prediction_df = pred_df_m
    #label_df = pts_level_char_df

    patient_IDs = list(set(prediction_df['study_id']))
    
    pt_level_pred_list = []
    ct = 0
    for pt in patient_IDs:     
        if ct % 500 == 0 :
            print(ct)
        ct += 1
        
        #Load pt data
        cur_pred_df = prediction_df[prediction_df['study_id'] == pt].copy()
        cur_label_df = label_df[label_df['study_id'] == pt].copy()
    
    
        #Convert month column to date object
        cur_pred_df = reformat_month_col_one_pt(cur_pred_df)
        
        #Get predicted month and label
        if pred_method == "3month":
            cur_pred_month = prediction_sbce_month_consecutiveMethod(cur_pred_df,threshold)
        elif pred_method == "1month":
            cur_pred_month = prediction_sbce_month_1stMonthMethod(cur_pred_df,threshold)
        
        if cur_pred_month is not None:
            cur_pred_label = 1
        else:
            cur_pred_label = 0
    
        #Get SBCE label and acutal SBCE month
        cur_sbce_label = cur_label_df[SBCE_Column].item()
        
        if cur_sbce_label == 1:
            cur_sbce_eaxct_day = cur_label_df['Date_2nd_Event'].item()
            #replace the exact day as the 1st of day of month to compare , cuz our algorithm do not go for the exact day
            cur_sbce_month = cur_sbce_eaxct_day.replace(day=1)
            
        else: 
            cur_sbce_eaxct_day = None
            cur_sbce_month = None
            
        cur_pred_df_ptlevel = pd.DataFrame({'study_id':pt,
                                            'actual_label': cur_sbce_label,
                                            'actual_exact_monthday':cur_sbce_eaxct_day,
                                            'actual_month':cur_sbce_month,
                                            'pred_label': cur_pred_label,
                                            'pred_month': cur_pred_month}, index = [0])

        pt_level_pred_list.append(cur_pred_df_ptlevel)

    pt_level_pred_df = pd.concat(pt_level_pred_list, axis = 0)

    #Compute month diff
    pt_level_pred_df['RAW_Month_Diff'] = (pt_level_pred_df['pred_month'] - pt_level_pred_df['actual_month'])/np.timedelta64(1, 'M')
    pt_level_pred_df['ABS_Month_Diff'] = abs(pt_level_pred_df['RAW_Month_Diff'])

    return pt_level_pred_df
