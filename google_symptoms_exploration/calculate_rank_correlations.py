#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Jan 20 18:05:11 2021

@author: jingjingtang
"""
from datetime import timedelta
from scipy.stats import spearmanr

import warnings
warnings.filterwarnings('ignore')

import pandas as pd
import numpy as np

symptom_sets = {
    "Respiratory": ["symptom:Cough", "symptom:Phlegm", "symptom:Shortness of breath", "symptom:Wheeze",
                        "symptom:Asthma", "symptom:Pneumonia", "symptom:Sore throat"],
    "Sensory": ["symptom:Anosmia", "symptom:Dysgeusia", "symptom:Ageusia"],
    "Fever": ["symptom:Fever", "symptom:Hyperthermia", "symptom:Chills", "symptom:Shivering"],
    "Fatigue": ["symptom:Fatigue", "symptom:Weakness"],
    "GI": ["symptom:Nausea", "symptom:Vomiting", "symptom:Diarrhea", "symptom:Indigestion",
          "symptom:Abdominal pain"],
    "Pain": ["symptom:Pain", "symptom:Sharp pain", "symptom:Chest pain", "symptom:Myalgia",
             "symptom:Arthralgia", "symptom:Eye pain", "symptom:Headache", "symptom:Migraine",
             "symptom:Cramp"]
}

all_symptoms = []
for setn in symptom_sets.keys():
    all_symptoms.extend(symptom_sets[setn])
all_symptoms = sorted(all_symptoms)




def calculate_rawsum_corr(part_df):
    try:
        part_df["computed"] = part_df["coef"].values[-1] * part_df["raw"] + part_df["intercept"].values[-1]
        corr = spearmanr(part_df[["computed", "cases"]].values,
                          nan_policy="omit")[0]
    except:
        corr = np.nan
    return corr

def calculate_regression_corr(part_df):
    try:
        computed = 0
        for col in part_df.columns:
            if "coef" in col:
                sym = col.split("_")[1]
                computed += part_df[col].values[-1] * part_df["raw_" + sym].values
        computed += part_df["intercept"].values[-1]
        part_df["computed"] = computed

        corr = spearmanr(part_df[["computed", "cases"]].values,
                          nan_policy="omit")[0]
    except:
        corr = np.nan
    return corr

def create_timewise_corr(setn, method, fillmissingness):
    # For each symptom
    
    corr_df = pd.DataFrame(columns=["county", "symptom_set", "correlation", "date"])        
    print(setn) 
    subdf = pd.read_csv("./sensorization_with_as_of/sensored/%s_%s_%s_sensorization_12_14.csv"%(setn, method, fillmissingness), 
                     parse_dates=["date"]).dropna()
    geo_list = subdf["geo_id"].unique()
    
    print("%d counties in total"%len(geo_list))
    num = 0
    for county in set(geo_list):
        num += 1
        if num % 10 == 0:
            print("%d counties finished"%num)
        local_df = subdf[subdf["geo_id"] == county]     
        start_date = subdf["date"].min()
        end_date = subdf["date"].max()
        n_days = (end_date - start_date).days + 1
        # Make sure the time index is expected
        timeindex = [start_date + timedelta(days=i) for i in range(n_days)]
        local_df = local_df.set_index("date").reindex(timeindex).reset_index()
        
        corrs = []
        for _d in timeindex:
            if (_d - start_date).days < 41:
                corrs.append(np.nan)
                continue
            part_df = local_df.loc[(local_df["date"] <= _d)
                                    & (local_df["date"] >= _d - timedelta(days=41))]
            if method == "rawsum":
                corrs.append(calculate_rawsum_corr(part_df))
            else:
                corrs.append(calculate_regression_corr(part_df))
        info_df = pd.DataFrame({"county": county,
                                "symptom_set": setn,
                                "correlation": corrs,
                                "date": local_df["date"]})
        corr_df = corr_df.append(info_df)
    return  corr_df 


def create_geowise_corr_final_as_of(setn, method, fillmissingness):
    # For each symptom
    
    corr_df = pd.DataFrame(columns=["symptom_set", "correlation", "date"])  
    k = 0
    print(setn) 
    df = pd.read_csv("./sensorization_with_as_of/sensored/%s_%s_%s_sensorization_12_14.csv"%(setn, method, fillmissingness), 
                     parse_dates=["date"]).dropna()
    
    # Calculate computed values using coef and intecept from
    # the most recent as of date
    for county in df["geo_id"].unique():
        intercept = df.loc[df["geo_id"] == county, "intercept"].values[-1]
        if method == "rawsum":
            coef = df.loc[df["geo_id"] == county, "coef"].values[-1]                    
            df.loc[df["geo_id"] == county, "computed"] = coef * df.loc[df["geo_id"] == county, "raw"].values + intercept
        else:
            computed = 0
            for col in df.columns:
                if "coef" in col:
                    sym = col.split("_")[1]
                    coef = df.loc[df["geo_id"] == county, col].values[-1]
                    computed += coef * df.loc[df["geo_id"] == county, "raw_" + sym].values
            computed += intercept
            df.loc[df["geo_id"] == county, "computed"] = computed
                    
    n_days = df["date"].unique().shape[0]
    date_list = [df["date"].min() + timedelta(days=i) for i in range(n_days)]
    
    print("%d dates in total"%len(date_list))
    for _d in date_list:
        print(_d)
        part_df = df[df["date"] == _d]
        try:
            corr = spearmanr(part_df[["computed", "cases"]].values,
                              nan_policy="omit")[0]
        except:
            corr = np.nan
        corr_df.loc[k] = [setn, corr, _d]
        k += 1
    return  corr_df 

def create_geowise_corr_varying_as_of(setn, method, fillmissingness):
    # For each symptom
    
    corr_df = pd.DataFrame(columns=["symptom_set", "correlation", "date"])  
    k = 0
    df = pd.read_csv("./sensorization_with_as_of/sensored/%s_%s_%s_sensorization_12_14.csv"%(setn, method, fillmissingness), 
                     parse_dates=["date"]).dropna()
                    
    n_days = df["date"].unique().shape[0]
    date_list = [df["date"].min() + timedelta(days=i) for i in range(n_days)]
    
    print("%d dates in total"%len(date_list))
    for _d in date_list:
        part_df = df[df["date"] == _d]
        try:
            corr = spearmanr(part_df[["predicted", "cases"]].values,
                              nan_policy="omit")[0]
        except:
            corr = np.nan
        corr_df.loc[k] = [setn, corr, _d]
        k += 1
    return  corr_df 


symptom_sets = ["fatigue", "fever", "gi", "respiratory", "pain", "sensory"]
for fillmissingness in ["fill0", "fillminratio"]:
    for method in ["rawsum", "regression"]:
        for setn in symptom_sets:
            corr_df = create_geowise_corr_final_as_of(setn, method, fillmissingness)
            corr_df.to_csv("./sensorization_with_as_of/geo_wise_final_as_of/%s_static_geowise_corr_df_%s_%s.csv"%(setn, method, fillmissingness), index=False)