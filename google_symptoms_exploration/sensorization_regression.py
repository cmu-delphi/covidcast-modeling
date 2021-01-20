#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sun Jan 17 15:14:28 2021

@author: jingjingtang
"""
import argparse
from datetime import datetime, timedelta

import pandas as pd
import numpy as np

from sklearn.linear_model import LinearRegression

parser = argparse.ArgumentParser( formatter_class=argparse.ArgumentDefaultsHelpFormatter )
parser.add_argument( "--setn", type = str)
parser.add_argument( "--fillmissingness", type = str)

args = parser.parse_args() 


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
             "symptom:Cramp"],
    "Combined":['symptom:Allergy', 'symptom:Infection', 'symptom:Shortness of breath', 'symptom:Diarrhea',
                'symptom:Hyperthermia', 'symptom:Rhinorrhea', 'symptom:Wheeze', 'symptom:Cramp',
                'symptom:Nasal congestion', 'symptom:Dysgeusia', 'symptom:Vomiting', 'symptom:Anxiety',
                'symptom:Rhinitis', 'symptom:Shivering', 'symptom:Inflammation', 'symptom:Common cold',
                'symptom:Arthralgia', 'symptom:Fatigue', 'symptom:Pneumonia', 'symptom:Sleep disorder',
                'symptom:Abdominal pain', 'symptom:Sharp pain', 'symptom:Cough', 'symptom:Myalgia',
                'symptom:Ageusia', 'symptom:Migraine', 'symptom:Phlegm', 'symptom:Chest pain',
                'symptom:Headache', 'symptom:Asthma', 'symptom:Fever', 'symptom:Indigestion',
                'symptom:Eye pain', 'symptom:Anosmia', 'symptom:Pain', 'symptom:Weakness', 'symptom:Nausea',
                'symptom:Sinusitis', 'symptom:Sore throat', 'symptom:Chills', 'symptom:Hypothyroidism']
}


for set_name in symptom_sets.keys():
    print("- %s:"%set_name)
    for gs in symptom_sets[set_name]:
        print(gs)
    print("\n")
    

if args.fillmissingness == "fillminratio":
    gs_df = pd.read_csv("./raw/county_fillminratio_raw_google_symptoms_allcovid_12_14.csv", parse_dates=["date"])
else:
    gs_df = pd.read_csv("./raw/county_google_symptoms_allcovid_12_14.csv", parse_dates=["date"])
n_days = (gs_df["date"].max() - gs_df["date"].min()).days + 1

gs_df = gs_df[~gs_df["geo_id"].isnull()]
gs_df["geo_id"] = [str(int(x)).zfill(5) for x in gs_df["geo_id"]]


# Filter out counties with more than 50% days unavailable
SYMPTOM_COUNTY_SET = {}
for idx in range(2, gs_df.columns.shape[0]):
    symptom = gs_df.columns[idx].split(":")[1]
    subdf = gs_df[[gs_df.columns[idx], "geo_id", "date"]].dropna()
    subdf.loc[subdf[gs_df.columns[idx]] == 0, gs_df.columns[idx]] = np.nan
    mask_df = subdf.groupby("geo_id").count()
    
    geo_list = set(mask_df[mask_df[gs_df.columns[idx]] >= n_days*0.5].index.values)
    
    SYMPTOM_COUNTY_SET[gs_df.columns[idx]] = geo_list
    
api_df = pd.read_csv("./raw/county_adjusted_usafacts_smoothed_case_rates_12_14.csv", parse_dates=["date", "adjusted_as_of"])
api_df["geo_id"] = [str(int(x)).zfill(5) for x in api_df["geo_id"]]
api_df = api_df.sort_values(["date", "adjusted_as_of", "geo_id"])


api_df_dict = {}
for _d in api_df["date"].unique():
    api_df_dict[_d] = api_df.loc[api_df["adjusted_as_of"] == _d]


# Read smoothed gs
if args.fillmissingness == "fillminratio":
    gs_df = pd.read_csv("./raw/county_fillminratio_smoothed_google_symptoms_allcovid_12_14.csv", parse_dates=["date"])
else:
    gs_df = pd.read_csv("./raw/county_smoothed_google_symptoms_allcovid_12_14.csv", parse_dates=["date"])
gs_df["geo_id"] = [str(x).zfill(5) for x in gs_df["geo_id"]]
    

def static_sensorization_for_regression(setn, api_df_dict, gs_df): 
    """
    Statis sensorization for google symptom sets indicators which are
    created based on regression

    Parameters
    ----------
    setn : str
        the name of symptom sets
    api_df_dict : dict
        keys are (county, date), where date is for adjusted as_of date
    gs_df : pd.DataFrame
        dataframe for google symptoms.

    """
    coef_list = ["coef_" + x.split(":")[1] for x in symptom_sets[setn]]
    raw_list = ["raw_" + x.split(":")[1] for x in symptom_sets[setn]]
    regression_df = pd.DataFrame(columns=["date", "geo_id", "SetName"] \
                             + coef_list + raw_list + ["intercept", "predicted", "cases"])
    j = 0
    print(setn)
          
    
            
    # Train model for each county        
    geo_set = set([])
    for sym in symptom_sets[setn]:
        geo_set = geo_set.union(SYMPTOM_COUNTY_SET[sym])               
    print("%d counties in total."%len(geo_set))
    num = 0
    for county in geo_set:
        num+=1
        if num%5 == 0:
            print("%d counties finished"%num)
            
        local_df = gs_df[gs_df["geo_id"] == county]      
        X = local_df[symptom_sets[setn]].values
        date_list = local_df["date"].values

        # static regression
        # using entire past used to train lr model for each symptom set
        for i in range(7, X.shape[0]):
            
            _date = date_list[i]
            
            # Select target value with data available before date i for each county 
            target_df = api_df_dict[_date] 
            target_df = target_df[target_df["geo_id"] == county]
            y = target_df["cases"].values
            
            # Train the model
            reg = LinearRegression()           
            reg.fit(X[:i-7+1,:] , y[:i-7+1])
                        
            result = [_date, county, setn]
            result.extend( list(reg.coef_))
            result.extend( X[i:i+1, :][0])
            result.append(reg.intercept_)               
            result.append(reg.predict(X[i:i+1, :])[0])
            result.append(y[i])

            regression_df.loc[j] = result
            j+=1

    return regression_df

result = static_sensorization_for_regression(args.setn, api_df_dict, gs_df)

result.to_csv("./output/%s_regression_%s_sensorization_12_14.csv"%(args.setn, args.fillmissingness), index=False)