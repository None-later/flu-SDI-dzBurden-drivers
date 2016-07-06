#!/usr/bin/python

##############################################
###Python template
###Author: Elizabeth Lee
###Date: 7/5/16
###Function: Check data quality of base ILI data, after mySQL export, Python export, and R export, for use in INLA modeling

###Import data: dz_burden/SQL_export/ILIViz_allWeekly_totServ_totAge_allZip.csv; dz_burden/SQL_export/pop_allYearly_totAge_allZip.csv

###Command Line: python snippet_check_dataQuality_mysqlILI.py
##############################################


### notes ###


### packages ###
import pandas as pd

### functions ###
def zip3string(series):
	new_string = "00" + series['zip3_o']
	return new_string[-3:] 

### original ili data ###
iliin = '/home/elee/Dropbox/Elizabeth_Bansal_Lab/SDI_Data/dz_burden/SQL_export/ILIViz_allWeekly_totServ_totAge_allZip.csv'

all_df = pd.read_table(iliin, skiprows=1, sep=',', parse_dates=['week'], names=['week', 'zip3_o', 'ili', 'viz'], dtype={'zip3_o':'object'})
print all_df.dtypes
print all_df.isnull().sum() # there are no NaN

all_df['zip3'] = all_df.apply(zip3string, axis='columns')
all_df2 = all_df.set_index(['week', 'zip3'])

## perform checks on zip3 007 ##
# not present in mysql database --> NaN in the pivot table
print all_df2.loc[pd.IndexSlice[:, '007'],['ili', 'viz']]
ili_df = all_df.pivot(index='week', columns='zip3', values='ili')
print ili_df.loc[:, ['007']]
viz_df = all_df.pivot(index='week', columns='zip3', values='viz')
print viz_df.head()

### original pop data ###
popin = '/home/elee/Dropbox/Elizabeth_Bansal_Lab/SDI_Data/dz_burden/SQL_export/pop_allYearly_totAge_allZip.csv'
pop_dummy = pd.read_table(popin, skiprows=1, sep=',', parse_dates=['year'], names=['year', 'zip3_o', 'pop'], na_values=0, dtype={'zip3_o':'object'})
pop_dummy['zip3'] = pop_dummy.apply(zip3string, axis='columns')
pop_df = pop_dummy.pivot(index='year', columns='zip3', values='pop')
mask = pd.isnull(pop_dummy['pop'])
print pop_dummy[mask]

### R_export ILI data ###
inR = '/home/elee/Dropbox/Elizabeth_Bansal_Lab/SDI_Data/dz_burden/R_export/ilicByallZip3_allWeekly_totServ_totAge.csv'
R_df = pd.read_table(inR, sep=',', parse_dates=['Thu.week'], dtype={'zip3':'object'})
print R_df.head()

R_df2 = R_df.drop(['week', 'year', 'month'], axis=1).set_index(['Thu.week', 'zip3', 'flu.week', 'fit.week', 'incl.lm'])
# R_df2 = R_df.set_index(['Thu.week', 'zip3', 'flu.week', 'fit.week', 'incl.lm']).drop(['002'], axis=0, level='zip3') # remove certain rows according to multilevel indexing
idx = pd.IndexSlice
# dataframes with multilevel indexes need to be sorted fully before sliced: http://stackoverflow.com/questions/19981518/sorting-multi-index-to-full-depth-pandas
R_df2.sortlevel(axis=0, inplace=True, sort_remaining=True)
print R_df2.loc[idx[:,['007'],:,:,:],idx['ili','pop']]

# no coverage data for 2010 in any zip3s
R_df3 = R_df2.loc[idx[:,['010', '945'],:,:,:],idx['ili','alpha_z.y','cov_z.y','ILIc']]
mask = pd.isnull(R_df3['alpha_z.y'])
print R_df3[mask]
