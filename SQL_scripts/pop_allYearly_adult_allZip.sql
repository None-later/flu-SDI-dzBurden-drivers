/*----SQL TEMPLATE----
Author: Elizabeth Lee
Date: 11/10/16
Function: Export data for initial characterization of spatial heterogeneity in disease burden. Data included:
- population size
- year
- adults only: 20-29, 30-39, 40-49, 50-59, 60-69
- include zip3s

Command Line: mysql < pop_allYearly_adult_allZip.sql | sed 's/\t/,/g' > /home/elee/Dropbox/Elizabeth_Bansal_Lab/SDI_Data/dz_burden/SQL_export/pop_allYearly_adult_allZip.csv

Data: flu table: SDI
*/
use sdi;
SELECT year(flu.WEEK), flu.patient_zip3, flu.popstat FROM flu
	WHERE flu.SERVICE_PLACE = "TOTAL" and (flu.AGEGROUP = "20-29 years" or flu.AGEGROUP = "30-39 years" or flu.AGEGROUP = "40-49 years" or flu.AGEGROUP = "50-59 years" or flu.AGEGROUP = "60-69 years") AND flu.patient_zip3 <> "TOT"
	GROUP BY year(flu.WEEK), flu.patient_zip3
;





