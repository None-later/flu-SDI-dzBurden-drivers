#!/usr/bin/python

##############################################
###Python template
###Author: Elizabeth Lee
###Date: 04/07/15
###Purpose: data cleaning and processing for disease burden analyses 

# (import functions_dataProcessing as fdp)

###Import data: 

###Command Line: wouldn't be called directly from command line
##############################################
## index of functions ##

# SDIweek (init: 4/7/15; last updated: 4/7/15)
# weeklyZip3_ILI_processing (4/7/15; )
# yearlyZip3_pop_processing (4/7/15)
# generate_allzip3s (5/3/15)
# generate_allseasons (5/3/15)
# generate_weeklist (5/3/15)
# define_studyZip3s (5/3/15)

##############################################
## header ##
import csv
from datetime import date
import pandas as pd

##############################################
## global parameters ##
gp_plotting_seasons = range(2,10) # season numbers for which data will be plotted (eg. Season 2 = 2001-02)
gp_epimonths = [10, 11, 12, 1, 2, 3, 4, 5]
gp_nonepimonths = [6, 7, 8, 9]

##############################################
## call parameters ##
pseasons = gp_plotting_seasons

##############################################
def SDIweek(datetimeWeek):
	''' Process Sunday date in SDI data to return Thursday date, correct season number, and week number, where season 0 is 1999-2000 flu season.
	'''
	Thu_date = datetimeWeek + timedelta(days=4) # Sun+4days=Thursday
	year, weeknum, _ = Thu_date.isocalendar() # yr, wknum, day
	if weeknum >= 40:
		season = year+1-2000
	else:
		season = year-2000

	return Thu_date, int(season), weeknum

##############################################
def weeklyZip3_ILI_processing(csvfile_ILIViz):
	''' Import SQL_export/ILIViz_allWeekly_totServ_totAge_allZip.csv. 'SDIweek' is nested. Return two dicts:
	dict_wk_seasWknum[Thu date] = (season number, week number)
	dict_wkZip_ILIViz[(Thu date, zip3)] = (ili count, any diagnosis count)
	'''
	main(weeklyZip3_ILI_processing)

	## convert file to csv object ##
	csvObject_ILIViz = csv.reader(csvfile_ILIViz, delimiter=',')

	## import ILI/viz data from csv object ##
	dict_wk_seasWknum, dict_wkZip_ILIViz = {}, {}
	for row in csvObject_ILIViz:
		week = row[0]
		zip3 = str(row[1])
		ili, viz = int(row[2]), int(row[3])
		Sun_dt = date(int(week[:4]), int(week[5:7]), int(week[8:]))
		Thu_dt, seas, wknum = SDIweek(Sun_dt)
		dict_wk_seasWknum[Thu_dt] = (seas, wknum)
		dict_wkZip_ILIViz[(Thu_dt, zip3)] = (ili, viz)

	return dict_wk_seasWknum, dict_wkZip_ILIViz

##############################################
def yearlyZip3_pop_processing(csvfile_pop):
	''' Import SQL_export/pop_allYearly_totAge_allZip.csv. Return one dict:
	dict_yrZip_pop[(year, zip3)] = population
	'''
	main(yearlyZip3_pop_processing)
	
	## convert file to csv object ##
	csvObject_pop = csv.reader(csvfile_pop, delimiter=',')

	## import pop data from csv object ##
	dict_yrZip_pop = {}
	for row in csvObject_pop:
		year, zip3 = int(row[0]), str(row[1])
		pop = int(row[2])
		dict_yrZip_pop[(year, zip3)] = pop

	return dict_yrZip_pop

##############################################
# def define_flu_season(dict_wk_seasWknum, dict_wkZip_ILIViz, dict_yrZip_pop):
# 	''' Define flu season period within epidemic weeks for each zip3.
# 	'''
# 	main(define_flu_season)

##############################################
def generate_allzip3s(dict_wkZip_ILIViz):
	''' Generate list of all zip3s in the dataset.
	'''
	return sorted(set([key[1] for key in dict_wkZip_ILIViz]))

##############################################
def generate_allseasons(dict_wk_seasWknum):
	''' Generate list of all seasons in the dataset.
	'''
	return sorted(set([dict_wk_seasWknum[date][0] for date in dict_wk_seasWknum]))


##############################################
def generate_weeklist(dict_wk_seasWknum):
	''' Generate list of weeks for each possible epidemic (October to May) season and non-epidemic season (June to September prior to flu season).
	dict_seas_epiweek_ls[s] = [Oct wk1, .. May wk4]
	dict_seas_nonepiweek_ls[s] = [Jun wk1, ... Sep wk4]
	'''
	main(generate_weeklist)

	dict_seas_epiweek_ls, dict_seas_nonepiweek_ls = defaultdict(list), defaultdict(list)

	allseasons = generate_allseasons(dict_wk_seasWknum)

	for s in pseasons:
		epiweeks = sorted(set([week for week in dict_wk_seasWknum if dict_wk_seasWknum[week][0] == s and week.month in gp_epimonths]))
		nonepiweeks = sorted(set([week for week in dict_wk_seasWknum if dict_wk_seasWknum[week][0] == (s-1) and week.month in gp_nonepimonths]))
		dict_seas_epiweek_ls[s] = epiweeks
		dict_seas_nonepiweek_ls[s] = nonepiweeks

	return dict_seas_epiweek_ls, dict_seas_nonepiweek_ls

##############################################
def define_studyZip3s(dict_wkZip_ILIViz, dict_seas_epiweek_ls):
	''' For each season, generate the list of zip3s that have data for all epidemic period (Oct to May) weeks.
	dict_seas_zip3_ls[seas] = [zip3 1, zip3 2, ...]
	'''
	main(define_studyZip3s)

	allseasons = generate_allseasons(dict_wk_seasWknum)
	allzip3s = generate_allzip3s(dict_wkZip_ILIViz)

	dict_seas_zip3_ls = defaultdict(list)
	
	# for each season, grab all zip3s that have an entry in dict_wkZip_ILIViz for all weeks in d_seas_epiweek_ls
	for s in pseasons:
		epiweeks = dict_seas_epiweek_ls[s]
		# subset ILI data to include only data with epidemic weeks in season s
		dict_wkZip_ILIViz_subset = dict((k, dict_wkZip_ILIViz[k]) for k in dict_wkZip_ILIViz if k[0] in epiweeks)
		
		# identify zip3s with all epiweeks
		zip3_dummy = []
		for z in allzip3s:
			if len([key for key in dict_wkZip_ILIViz_subset if key[1] == z]) == len(epiweeks):
				zip3_dummy.append(z)

		dict_seas_zip3_ls[s] = sorted(zip3_dummy)

	return dict_seas_zip3_ls

##############################################


##############################################
# footer

def main(function):
	print 'Running', __name__, function.__name__

if __name__ == '__main__':
	print 'Executed from the command line'
	main()