#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Mar 25 20:38:10 2021

@author: lucasliu
"""


import sqlite3 as sql
import pandas as pd


data_dir = "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Data/Testing data for UH3 - Dec 16 2020/"
medicare_df = pd.read_csv(data_dir + "kcr_medicare_claims_fb0015.csv")
conn = sql.connect('kcr_medicare_claims_fb0015.db')
medicare_df.to_sql('medicare_claims', conn)

