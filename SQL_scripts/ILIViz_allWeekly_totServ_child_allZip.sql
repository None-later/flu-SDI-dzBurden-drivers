/*----SQL TEMPLATE----
Author: Elizabeth Lee
Date: 11/10/16
Function: Export data for initial characterization of spatial heterogeneity in disease burden. Data included:
- ILI cases
- all visits
- all weeks in dataset
- weekly data
- combined service place
- children only: 5-9, 10-14, 15-19 years old
- include zip3s

Command Line: mysql < ILIViz_allWeekly_totServ_child_allZip.sql | sed 's/\t/,/g' > /home/elee/Dropbox/Elizabeth_Bansal_Lab/SDI_Data/dz_burden/SQL_export/ILIViz_allWeekly_totServ_child_allZip.csv

Data: flu table: SDI
*/
use sdi;

SELECT flu.WEEK, flu.patient_zip3, flu.ILI_m, flu.ANY_DIAG_VISIT_CT
	FROM flu 
	WHERE flu.SERVICE_PLACE = "TOTAL" and (flu.AGEGROUP = "2-4 years" or flu.AGEGROUP = "5-9 years" or flu.AGEGROUP = "10-14 years") AND flu.patient_zip3 <> "TOT"
	GROUP BY flu.WEEK, flu.patient_zip3
;





