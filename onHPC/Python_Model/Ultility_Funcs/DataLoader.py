#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Feb 20 19:33:42 2023

@author: lucasliu
"""

import pyreadr


def load_rdata(indir,filename,file_keyname,label_column):
    df = pyreadr.read_r(indir + filename)
    df = df[file_keyname]
    df_Y = df[label_column]
    df_ID = df[["study_id","sample_id"]]    
   
    #Mkae sure these columns are not in the X part for training
    df_X = df.drop(columns=["study_id","sample_id","y_PRE_OR_POST_2ndEvent",
                            "y_PRE_OR_POST_2ndEvent_ExcludedDeath"], 
                   errors = "ignore") #If ‘ignore’, suppress error and only existing labels are dropped.
    
    
    return df_X,df_Y,df_ID
