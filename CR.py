#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Dec 17 15:53:02 2020

@author: lucasliu
"""


# import required module 
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import os


def intersection(lst1, lst2): 
    lst3 = [value for value in lst1 if value in lst2] 
    return lst3 

def get_codes(col_names,data_input):
    #col_names = diag_col_names
    #data_input = medicaid_df

    all_unique_codes_list = []
    for d in col_names:
        curr_Col_unique_codes = list(set(data_input[d].dropna().tolist())) #drop NAs and unique codes of this column
        all_unique_codes_list.append(curr_Col_unique_codes)
    
    all_unique_codes = []
    #flaten list of code list to one code list
    for sublist in all_unique_codes_list:
        for code in sublist:
            all_unique_codes.append(code)
    
    updated_all_unique_codes = list(set(all_unique_codes)) #unique codes of all columns
        
    return updated_all_unique_codes


data_dir = "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Data/Testing data for UH3 - Dec 16 2020/"
outdir = "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Intermediate_Data/"


#KCR Data
KCR_df = pd.read_csv(data_dir + "uh3_kcrdata.csv")
KCR_IDs = KCR_df['study_id'].unique()
len(KCR_IDs) #47130 


############################################################################
###################  medicaid                            ###################
############################################################################
medicaid_df_healthClaims = pd.read_csv(data_dir + "kcr_medicaid_healthclaims_fb0015.csv",parse_dates=["DTE_FIRST_SVC","DTE_LAST_SVC"])
medicaid_df_PHARMCLAIMS = pd.read_csv(data_dir + "KCR_MEDICAID_PHARMCLAIMS_FB0015.csv")


#Get unique IDs
medicaid_ids = medicaid_df_healthClaims["study_id"].dropna().unique()
len(medicaid_ids) #12959
len(medicaid_df_healthClaims["Id_medicaid"].unique()) #12964

medicaid_ids2 = medicaid_df_PHARMCLAIMS["study_id"].dropna().unique()
len(medicaid_ids2) #10656
len(medicaid_df_PHARMCLAIMS["Id_medicaid"].unique()) #10659

len(intersection(medicaid_df_healthClaims["Id_medicaid"].unique(),medicaid_df_PHARMCLAIMS["Id_medicaid"].unique())) #10596

#Get all Unique diagnose code
#ICD diagnosis code: CDE_DIAG_PRIM , cde_diag_2-cde_diag_4
diag_col_names = ["CDE_DIAG_" + str(x)  for x in range(2, 5)] + ["CDE_DIAG_PRIM"]
unique_diag_codes_medicaid = get_codes(diag_col_names,medicaid_df_healthClaims)
len(unique_diag_codes_medicaid) #23063

#output
unique_diag_codes_df = pd.DataFrame(unique_diag_codes_medicaid,columns=['Diag_Code'] )
unique_diag_codes_df.to_csv( outdir + "medicaid_unique_diag_codes.csv")

##Get all unqiue HCPCS procedure code
proc_col_names = ["CDE_PROC_PRIM"]
unique_proc_codes_medicaid = get_codes(proc_col_names,medicaid_df_healthClaims)
len(unique_proc_codes_medicaid) # 10276
unique_proc_codes_df = pd.DataFrame(unique_proc_codes_medicaid, columns = ['HCPCS_Proc_Code'])
unique_proc_codes_df.to_csv(outdir + "medicaid_unique_proc_codes.csv")

##Get all unqiue drug code
drug_col_names = ["CDE_NDC","CDE_THERA_CLS_AHFS"]
unique_drug_codes_medicaid = get_codes(drug_col_names,medicaid_df_PHARMCLAIMS)
len(unique_drug_codes_medicaid) # 33331
unique_drug_codes_df = pd.DataFrame(unique_drug_codes_medicaid, columns = ['Drug_Code'])
unique_drug_codes_df.to_csv(outdir + "medicaid_unique_drug_codes.csv")


# #For each patient, compute number of days they have records
# record_duration_list = []
# n_unique_diag_codes_list=[]
# n_unique_proc_codes_list = []
# ct = 0
# for pt in medicaid_ids:
#     if ct % 100 == 0:
#         print(ct)
#     curr_df = medicaid_df.loc[medicaid_df['study_id']== pt]
#     first_record_time = curr_df['DTE_FIRST_SVC'].min()
#     last_record_time = curr_df['DTE_FIRST_SVC'].max()
#     record_duration = pd.Timedelta(last_record_time - first_record_time, unit='d')
#     record_duration = record_duration.total_seconds()/(60*60*24) #days in float
#     record_duration_list.append(record_duration)
    
#     #number of unique records each patients:
#     n_unique_diag_codes = len(curr_df['CDE_DIAG_PRIM'].unique())
#     n_unique_diag_codes_list.append(n_unique_diag_codes)
    
#     #number of unique records each patients:
#     n_unique_proc_codes = len(curr_df['CDE_PROC_PRIM'].unique())
#     n_unique_proc_codes_list.append(n_unique_proc_codes)
    
#     ct += 1

# np.mean(record_duration_list) #1346 days
# np.mean(n_unique_diag_codes_list) #31 codes
# np.mean(n_unique_proc_codes_list) #53 codes

# indices = [i for i, x in enumerate(record_duration_list) if x == 0] #max and min time the same, means only one record
# one_records_df = medicaid_df[medicaid_df["studyid"].isin(indices)]
# onerecord_Ids = one_records_df["studyid"]
#onerecord_Ids
#len(intersection(onerecord_Ids,KCR_IDs))

# #plot hist
# _ = plt.hist(record_duration_list, bins='auto')  # arguments are passed to np.histogram
# plt.title("Histogram of records days")
# plt.show()
    



#Combine KCR first diagnose date and KCR recurence to the medicaid dataset
#IDs also in KCR
common_IDs  = intersection(medicaid_ids,medicaid_ids2) #12959
len(common_IDs)
common_IDs2  = intersection(medicaid_ids2,KCR_IDs) #10656
len(common_IDs2)



############################################################################
###################  medicare                            ###################
############################################################################
colnames = ['DGNS_CD1', 'DGNS_CD2', 'DGNS_CD3', 'DGNS_CD4', 'DGNS_CD5',
              'DGNS_CD6','DGNS_CD7', 'DGNS_CD8', 'DGNS_CD9', 'DGNS_CD10' 
              'DGNS_CD11','DGNS_CD12', 'HCFASPCL', 'HCPCS_CD', 'NDC_CD', 
              'CLAIM_ID', 'dfile','LOSCNT', 'PRCDRCD1', 'PRCDRCD2', 'PRCDRCD3', 
              'PRCDRCD4', 'PRCDRCD5','PRCDRCD6', 'PRCDRCD7', 'PRCDRCD8', 'PRCDRCD9', 'PRCDRCD10',
              'PRCDRCD11', 'PRCDRCD12', 'PRCDRCD13', 'PRCDRCD14', 'PRCDRCD15',
              'PRCDRCD16', 'PRCDRCD17', 'PRCDRCD18', 'PRCDRCD19', 'PRCDRCD20',
              'PRCDRCD21', 'PRCDRCD22', 'PRCDRCD23', 'PRCDRCD24', 'PRCDRCD25',
              'DGNS_CD13', 'DGNS_CD14', 'DGNS_CD15', 'DGNS_CD16', 'DGNS_CD17',
              'DGNS_CD18', 'DGNS_CD19', 'DGNS_CD20', 'DGNS_CD21', 'DGNS_CD22',
              'DGNS_CD23', 'DGNS_CD24', 'DGNS_CD25', 'REV_CNTR', 'PROD_SRVC_ID',
              'DAYS_SUPLY_NUM', 'BN', 'GCDF_DESC', 'GNN', 'study_id', 'claims_date']
dtype_values = ['object']*len(colnames)
data_dtypes = dict(zip(colnames, dtype_values))
sepFile_dir = "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Intermediate_Data/Medicare_Claims_seperated/"

#Seperate large files into smaller one
# ct = 0 
# madicare_data_list = []
# for df_chunk in pd.read_csv(data_dir + "kcr_medicare_claims_fb0015.csv",chunksize=500000, dtype = data_dtypes):
#     print(ct)
#     df_chunk.to_csv(sepFile_dir + "Medicare_Claims_" + str(ct) + ".csv", index = False)
#     madicare_data_list.append(df_chunk)
#     ct += 1
    
#Load chucked data
files = os.listdir(sepFile_dir)
madicare_data_list = []
ct = 0 
for f in files:
    print(ct)
    dat = pd.read_csv(sepFile_dir + f, dtype = data_dtypes)
    madicare_data_list.append(dat)
    ct += 1


madicare_data = pd.concat(madicare_data_list)
#Get unique IDs
medicare_ids = madicare_data["study_id"].dropna().unique()
medicare_ids = [int(ids) for ids in medicare_ids] 
len(medicare_ids) #31316
medicareClaim_ids = madicare_data["CLAIM_ID"].dropna().unique()
len(medicareClaim_ids) #7196680

#Get all Unique diagnose code
#ICD diagnosis code: dgns_cd1 -dgns_cd25  
diag_col_names = ["DGNS_CD" + str(x)  for x in range(1, 26)]
unique_diag_codes_medicare = get_codes(diag_col_names,madicare_data)
print(len(unique_diag_codes_medicare)) #26202
unique_diag_codes_df = pd.DataFrame(unique_diag_codes_medicare,columns=['Diag_Code'] )
unique_diag_codes_df.to_csv( outdir + "madicare_unique_diag_codes.csv")

##Get all unqiue PRCDRCD1 – PRCDRCD25 and HCPCS procedure code
proc_col_names = ["PRCDRCD" + str(x)  for x in range(1, 26)] + ['HCPCS_CD']
unique_proc_codes_medicare = get_codes(proc_col_names,madicare_data)
print(len(unique_proc_codes_medicare)) # 15773
unique_proc_codes_df = pd.DataFrame(unique_proc_codes_medicare, columns = ['HCPCS_Proc_Code'])
unique_proc_codes_df.to_csv(outdir + "madicare_unique_proc_codes.csv")

##Get all unqiue NDC drug code
drug_col_names = ['NDC_CD', 'PROD_SRVC_ID'] #24713
unique_drug_codes_medicare = get_codes(drug_col_names,madicare_data)
print(len(unique_drug_codes_medicare)) # 

unique_drug_codes_df = pd.DataFrame(unique_drug_codes_medicare, columns = ['Drug_Code'])
unique_drug_codes_df.to_csv(outdir + "madicare_unique_drug_codes.csv")




#combine medicare and medicare
common_IDs  = intersection(medicare_ids,medicaid_ids.tolist()) 
len(common_IDs) #8589
common_IDs2 = intersection(medicaid_ids2.tolist(),common_IDs)
len(common_IDs2) #10594

both_IDs  = medicare_ids + medicaid_ids.tolist() + medicaid_ids2.tolist()
len(set(both_IDs)) #35713

both_diag = unique_diag_codes_medicare + unique_diag_codes_medicaid
len(set(both_diag)) #30185

both_proc = unique_proc_codes_medicare + unique_proc_codes_medicaid
len(set(both_proc)) #17727

both_proc = unique_drug_codes_medicare + unique_drug_codes_medicaid
len(set(both_proc)) #41273
