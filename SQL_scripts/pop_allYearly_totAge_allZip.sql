/*----SQL TEMPLATE----
Author: Elizabeth Lee
Date: 4/7/15
Function: Export data for initial characterization of spatial heterogeneity in disease burden. Data included:
- population size
- year
- combined age groups
- include zip3s

Command Line: mysql < pop_allYearly_totAge_allZip.sql | sed 's/\t/,/g' > /home/elee/Dropbox/Elizabeth_Bansal_Lab/SDI_Data/dz_burden/SQL_export/pop_allYearly_totAge_allZip.csv

Data: flu table: SDI
*/

SELECT year(flu.WEEK), flu.patient_zip3, flu.popstat FROM flu
	WHERE flu.SERVICE_PLACE = "TOTAL" and flu.AGEGROUP = "TOTAL" AND flu.patient_zip3 <> "TOT"
	GROUP BY year(flu.WEEK), flu.patient_zip3
;





