#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Mar 25 14:44:26 2022

@author: lucasliu
"""
import pandas as pd

#######################################################################
##############              Data dir                     ############## 
#######################################################################
#local
data_dir = "/Volumes/LJL_ExtPro/Data/ReCAPSE_Data/Testing data for UH3 - Dec 16 2020/"
outdir   = "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Intermediate_Data/0610_21/0_Codes/BeforeClean_UniqueCodes/"
intermedia_dir = "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Intermediate_Data/0610_21/"

#######################################################################
#### final IDs     ##############
#######################################################################
final_id_df = pd.read_excel(intermedia_dir + "9_FinalIDs_And_UpdatedPtsChar/9_Final_ID1_WithPossibleMonthsHasNoCodes.xlsx")
final_ids = list(set(final_id_df['study_id']))

#######################################################################
############################## Medicaid  ############################## 
#######################################################################
#Codes columns
ICD_diag_cols   = ["CDE_DIAG_PRIM","CDE_DIAG_2","CDE_DIAG_3","CDE_DIAG_4"]  #ICD 9 or ICD10
HCPCS_proc_cols = ["CDE_PROC_PRIM"]                                         #HCPCS
AHFS_drug_cols  = ["CDE_THERA_CLS_AHFS"]                                    #AHFS
NDC_drug_cols   = ["CDE_NDC"]                                               #NDC

#Data
data_df1 = pd.read_csv(data_dir + "kcr_medicaid_healthclaims_fb0015.csv")
data_df2 = pd.read_csv(data_dir + "KCR_MEDICAID_PHARMCLAIMS_FB0015.csv")


#PROC: HCPC
HCPC_df_medicaid = data_df1[["study_id"] + HCPCS_proc_cols]
HCPC_df_medicaid = HCPC_df_medicaid.rename(columns={"CDE_PROC_PRIM": "HCPCS"})
HCPC_df_medicaid = HCPC_df_medicaid[HCPC_df_medicaid["study_id"].isin(final_ids)]
HCPC_df_medicaid = HCPC_df_medicaid.reset_index(drop = True)



#######################################################################
############################## Medicare  ############################## 
#######################################################################
#Codes columns
ICD_diag_cols = ["DGNS_CD" + str(x) for x in list(range(1,26))]           #ICD9 or ICD10
HCPCS_proc_cols = "HCPCS_CD"                                              #HCPCS
ICD_procedure_cols = ["PRCDRCD" + str(x) for x in list(range(1,26))]     #ICD9 or ICD10
NDC_drug_cols = ["NDC_CD","PROD_SRVC_ID"]                                  #NDC
all_cols =  ["study_id"] + [HCPCS_proc_cols]

#Data
data_df3 = pd.read_csv(data_dir + "kcr_medicare_claims_fb0015.csv", usecols= all_cols)


#PROC: HCPC
HCPC_df_medicare = data_df3
HCPC_df_medicare = HCPC_df_medicare.rename(columns={"HCPCS_CD": "HCPCS"})
HCPC_df_medicare = HCPC_df_medicare[HCPC_df_medicare["study_id"].isin(final_ids)]
HCPC_df_medicare = HCPC_df_medicare.reset_index(drop = True)

#######################################################################
#### Comb HCPC
#######################################################################
comb_HCPC_df = pd.concat([HCPC_df_medicare,HCPC_df_medicaid], axis = 0)
comb_HCPC_df = comb_HCPC_df.reset_index(drop = True)

#######################################################################
#Count
#######################################################################
cond = comb_HCPC_df["HCPCS"].isin(["C9131","J9354","J9355"])
Code_check = comb_HCPC_df[cond]
len(set(Code_check['study_id'])) #1086
len(final_ids) #18239
