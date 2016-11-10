#!/usr/bin/python

##############################################
###Python template
###Author: Elizabeth Lee
###Date: 11/10/16
###Function: Write ILI, visits, and population data for Aim 2.1 to file after manipulation with Pandas -- child data only

###Import data: dz_burden/SQL_export/ILIViz_allWeekly_totServ_child_allZip.csv; dz_burden/SQL_export/pop_allYearly_child_allZip.csv

###Command Line: python write_pandas_ili-viz-pop_child.py
##############################################


### notes ###


### packages ###
import csv
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

## local packages ##

### set these! ###
agegroup = 'child' # 'child' or 'adult'

### parameters ###

### functions ###
def ili_pandas_import(filename):
	all_dataframe = pd.read_table(filename, skiprows=1, sep=',', parse_dates=['week'], names=['week', 'zip3', 'ili', 'viz'])
	return all_dataframe

def pop_pandas_import(filename):
	all_dataframe = pd.read_table(filename, skiprows=1, sep=',', parse_dates=['year'], names=['year', 'zip3', 'pop'], na_values=0)
	return all_dataframe

### import data ###
iliin = '/home/elee/Dropbox/Elizabeth_Bansal_Lab/SDI_Data/dz_burden/SQL_export/ILIViz_allWeekly_totServ_%s_allZip.csv' % agegroup
popin = '/home/elee/Dropbox/Elizabeth_Bansal_Lab/SDI_Data/dz_burden/SQL_export/pop_allYearly_%s_allZip.csv' % agegroup

### program ###
all_df = ili_pandas_import(iliin)
ili_df = all_df.pivot(index='week', columns='zip3', values='ili')
viz_df = all_df.pivot(index='week', columns='zip3', values='viz')

pop_dummy_df = pop_pandas_import(popin)
pop_df = pop_dummy_df.pivot(index='year', columns='zip3', values='pop')

ili_viz_df = ili_df/viz_df

# export ili, viz, and pop to csv
ili_df.to_csv('/home/elee/Dropbox/Elizabeth_Bansal_Lab/SDI_Data/dz_burden/Py_export/iliByallZip_allWeekly_totServ_%s.csv' % agegroup, na_rep="NA")
# viz_df.to_csv('/home/elee/Dropbox/Elizabeth_Bansal_Lab/SDI_Data/dz_burden/Py_export/vizByallZip_allWeekly_%s_totAge.csv' % agegroup, na_rep="NA")
pop_df.to_csv('/home/elee/Dropbox/Elizabeth_Bansal_Lab/SDI_Data/dz_burden/Py_export/popByallZip_allYearly_%s.csv' % agegroup, na_rep="NA")
# ili_viz_df.to_csv('/home/elee/Dropbox/Elizabeth_Bansal_Lab/SDI_Data/dz_burden/Py_export/iliPropByallZip_allWeekly_%s_totAge.csv' % agegroup, na_rep="NA")


# 7/27/16 written to file

# dummy code for plotting
plt.figure(); ili_df.loc[:, 200:210].plot()
plt.show()