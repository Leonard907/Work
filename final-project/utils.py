#%%
import numpy as np
import pandas as pd
import matplotlib as mpl
import matplotlib.pyplot as plt
from matplotlib import rc
import seaborn as sns
from operator import add
from sklearn import datasets, linear_model
from regressors import stats

DATASETS_DIR=[
    './datasets/2014-2015.csv', 
    './datasets/2015-2016.csv', 
    './datasets/2016-2017.csv', 
    './datasets/2017-2018.csv', 
    './datasets/2018-2019.csv', 
    './datasets/2019-2020.csv' ]
LOCATION_DIR='datasets/LOCATION.csv'
RANKING_DIR='datasets/ranking.txt'
ENTRY_DIR='datasets/entry.txt'
ACDM_INVEST_DIR='datasets/academic_investment.txt'
FCLT_INVEST_DIR='datasets/facilities_investment.txt'

ENGLAND, SCOTLAND, WALES, NORTHERN_IRELAND = 0,1,2,3
ENGLAND_STR, SCOTLAND_STR, WALES_STR, NORTHERN_IRELAND_STR= 'England', 'Scotland', 'Northern Ireland', 'Wales',

uni_location = pd.read_csv(LOCATION_DIR)

read_list_file_drop_nl = lambda dir : list(map(lambda x: x[:-1], open(dir,'r').readlines()))
read_list_file_drop_nl_percent = lambda dir : list(map(lambda x: x[:-2], open(dir,'r').readlines()))
uni_ranking = read_list_file_drop_nl(RANKING_DIR)
# list(map(lambda x: x[:-1], open(RANKING_DIR,'r').readlines()))
entry_score_raw = read_list_file_drop_nl_percent(ENTRY_DIR) 
academic_invest_raw = read_list_file_drop_nl_percent(ACDM_INVEST_DIR)
facility_invest_raw = read_list_file_drop_nl_percent(FCLT_INVEST_DIR)

years = [2014, 2015, 2016, 2017, 2018, 2019]

degree_classifications_by_years = \
    dict([(years[i], pd.read_csv(DATASETS_DIR[i])) for i in range(len(years))])
degree_class_ratio_by_years = {}

# map function on degree_classifications_by_years
def map_on_deg_clsf_by_yr(funct):
    for year,_ in degree_classifications_by_years.items():
        funct(degree_classifications_by_years[year])

# map function and update value on degree_classifications_by_years
def update_val_deg_clsf_by_yr(funct):
    for year,_ in degree_classifications_by_years.items():
        degree_classifications_by_years[year] = funct(degree_classifications_by_years[year])

def parse(int_string):
    total = 0  # The resulting value
    cur = 1    # Current parse position
    int_string = str(int_string)
    for i in range(len(int_string)):
        if int_string[-1-i] != ',':  # Identify commas
            total += cur * int(int_string[-1-i])
            cur *= 10
    return total

def occurs(target, list):
    for item in list:
        if item in target or target in item:
            return list.index(item) + 1
    return False

map_on_deg_clsf_by_yr( lambda x : x.dropna(inplace=True))

for i in degree_classifications_by_years[2019].columns[2:]:
    for year,_ in degree_classifications_by_years.items():
        dataset = degree_classifications_by_years[year]
        dataset[i] = dataset[i].apply(parse)

update_val_deg_clsf_by_yr( lambda x : x.loc[x['First degree'] > 100])

for year,_ in degree_classifications_by_years.items():
    degree_classifications_by_years[year]['UKPRN'] = degree_classifications_by_years[year]['UKPRN'].astype('int64')


for k,v in degree_classifications_by_years.items():
    degree_class_ratio_by_years[k] = v.copy()

for year,_ in degree_class_ratio_by_years.items():
    dataset = degree_class_ratio_by_years[year]
    for i in degree_classifications_by_years[2019].columns[2:-1]:
            dataset[i] = dataset[i] / dataset['First degree']
    degree_class_ratio_by_years[year] = dataset.sort_values('First class honours')

#%%

ukprn=degree_class_ratio_by_years[2019].iloc[3]['UKPRN']
#%%
uni_geocode = pd.DataFrame(columns=['Name','Longitude','Latitude'])
index = 0
for _, row in degree_class_ratio_by_years[2019].iterrows():
    ukprn = row["UKPRN"]
    if len(uni_location[uni_location['UKPRN'] == ukprn]) > 0:
        longi = uni_location[uni_location['UKPRN'] == ukprn].iloc[0]['LONGITUDE'] 
        lati = uni_location[uni_location['UKPRN'] == ukprn].iloc[0]['LATITUDE']
        name = row['HE provider']
        uni_geocode.loc[index] = [name, longi, lati]
        index += 1

import reverse_geocoder as rg
def reverseGeocode(coordinates): 
    result = rg.search(coordinates)[0]['admin1']     
    # print(result)
    if result == ENGLAND_STR:
        return ENGLAND
    elif result == SCOTLAND_STR:
        return SCOTLAND
    elif result == WALES_STR:
        return WALES
    elif result == NORTHERN_IRELAND_STR:
        return NORTHERN_IRELAND



england = pd.read_csv('datasets/England.csv')
northern_ireland = pd.read_csv('datasets/Northern_Ireland.csv')
scotland = pd.read_csv('datasets/Scotland.csv')
wales = pd.read_csv('datasets/Wales.csv')

# %%
