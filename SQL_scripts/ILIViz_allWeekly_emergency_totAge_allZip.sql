/*----SQL TEMPLATE----
Author: Elizabeth Lee
Date: 7/26/16
Function: Export data for initial characterization of spatial heterogeneity in disease burden. Data included:
- ILI cases
- all visits
- all weeks in dataset
- weekly data
- emergency room and urgent care facilities
- combined age groups
- include zip3s

Command Line: mysql < ILIViz_allWeekly_emergency_totAge_allZip.sql | sed 's/\t/,/g' > /home/elee/Dropbox/Elizabeth_Bansal_Lab/SDI_Data/dz_burden/SQL_export/ILIViz_allWeekly_emergency_totAge_allZip.csv

Data: flu table: SDI
*/
use sdi;

SELECT flu.WEEK, flu.patient_zip3, flu.ILI_m, flu.ANY_DIAG_VISIT_CT
	FROM flu 
	WHERE flu.SERVICE_PLACE = "Emergency Room/Urgent Care Fac" and flu.AGEGROUP = "TOTAL" AND flu.patient_zip3 <> "TOT"
	GROUP BY flu.WEEK, flu.patient_zip3
;





