#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Feb 20 19:56:37 2023

@author: lucasliu
"""

from sklearn import metrics
from imblearn.metrics import specificity_score
from sklearn.metrics import accuracy_score, f1_score, precision_score, recall_score, confusion_matrix
import pandas as pd
import numpy as np



def compute_performance_binary(y_true,y_pred_prob,y_pred_class,thres_name):
    
    
    tn, fp, fn, tp = confusion_matrix(y_true, y_pred_class).ravel() #CM
    
    if y_pred_prob is not None:
        fpr_list, tpr_list, thresholds = metrics.roc_curve(y_true, y_pred_prob, pos_label=1)
        AUC = round(metrics.auc(fpr_list, tpr_list),2)
    else:
        AUC = pd.NA
    ACC = round(accuracy_score(y_true, y_pred_class),2)
    F1 = round(f1_score(y_true, y_pred_class),2)
    Recall = round(recall_score(y_true, y_pred_class),2)
    Precision = round(precision_score(y_true, y_pred_class),2)
    specificity = round(specificity_score(y_true, y_pred_class),2)
    npv = round(tn/(tn + fn),2)
    fpr = round(fp/(fp + tn),2) #1 - TNR
    fnr = round(fn/(fn + tp),2) #1 - TPR
    
    
    perf_tb = pd.DataFrame({"AUC": AUC, 
                            "ACC": ACC,
                            "Recall_Sensitivity_TPR": Recall,
                            "Specificity/TNR": specificity,
                            "Precision_PPV":Precision,
                            "F1": F1,
                            'NPV': npv,
                            "FPR": fpr,
                            "FNR": fnr,
                            "Threshold": thres_name},index = [0])
    
    return perf_tb


def compute_month_diff_perf(prediction_df,m_diff_col,threshold):
    prediction_df_SBCE = prediction_df[prediction_df['actual_label'] == 1]
    
    selected_col = m_diff_col + '_' + threshold #ABS_Month_Diff

    mean_v = prediction_df_SBCE[selected_col].mean()
    sd_v = prediction_df_SBCE[selected_col].std()
    median_v = prediction_df_SBCE[selected_col].median()
    q1_v = np.nanquantile(prediction_df_SBCE[selected_col],q = [0.25])
    q3_v = np.nanquantile(prediction_df_SBCE[selected_col],q = [0.75])
    min_v = prediction_df_SBCE[selected_col].min()
    max_v = prediction_df_SBCE[selected_col].max()
    
    #Num of correct predicted pateints
    cond = prediction_df_SBCE['pred_label_' + threshold] == prediction_df_SBCE['actual_label']
    num_pts = len(list(set(prediction_df_SBCE.loc[cond,'study_id'])))
    num_pts_actual = len(list(set(prediction_df_SBCE['study_id'])))
    
    
    new_col_name = m_diff_col
    diff_stats = pd.DataFrame({'Num_PTS_pred_correct_label':str(num_pts) + '/' + str(num_pts_actual),
                                'Mean_' + new_col_name: mean_v,
                               'SD_' + new_col_name: sd_v,
                               'Median_' + new_col_name: median_v,
                               'Q1_' + new_col_name: q1_v,
                               'Q3_'+ new_col_name: q3_v,
                               'Min_'+ new_col_name: min_v,
                               'Max_'+ new_col_name: max_v}, index = [0])
    
    return diff_stats
