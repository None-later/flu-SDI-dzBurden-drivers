#!/usr/bin/python

##############################################
###Python template
###Author: Elizabeth Lee
###Date: 5/3/15
###Function: plot centered zip3 ILI curves for each season to explore data and compare shapes of curves

###Import data: dz_burden/SQL_export/ILIViz_allWeekly_totServ_totAge_allZip.csv; dz_burden/SQL_export/pop_allYearly_totAge_allZip.csv

###Command Line: python explore_zip3ILIcurves.py
##############################################


### notes ###


### packages ###
import csv
import matplotlib.pyplot as plt
from collections import defaultdict

## local packages ##
import functions_dataProcessing as fxn

### data structures ###


### parameters ###

### functions ###



### import data ###
iliin = open('/home/elee/Dropbox/Elizabeth_Bansal_Lab/SDI_Data/dz_burden/SQL_export/ILIViz_allWeekly_totServ_totAge_allZip.csv','r')
ilin.readline() # remove header
popin = open('/home/elee/Dropbox/Elizabeth_Bansal_Lab/SDI_Data/dz_burden/SQL_export/pop_allYearly_totAge_allZip.csv','r')
popin.readline() # remove header

### program ###
d_wk_seasWknum, d_wkZip_ILIViz = fxn.weeklyZip3_ILI_processing(iliin)
d_yrZip_pop = fxn.yearlyZip3_pop_processing(popin)
d_seas_epiweek_ls, d_seas_nonepiweek_ls = fxn.generate_weeklist(d_wk_seasWknum)
d_seas_zip3_ls = fxn.define_studyZip3s(d_wkZip_ILIViz, d_seas_epiweek_ls)