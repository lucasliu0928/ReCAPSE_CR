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
    drop_cols  = ["study_id","sample_id",label_column]
    df_X  = df.drop(columns = drop_cols)
    
    return df_X,df_Y,df_ID
