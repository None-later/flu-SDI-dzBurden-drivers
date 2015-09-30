/*----SQL TEMPLATE----
Author: Elizabeth Lee
Date: 9/29/15
Function: Export state-level percentage uninsured for 2002-09 based on HI_CPSasec_state table.

Command Line: mysql < HI_state_0209.sql | sed 's/\t/,/g' > ../SQL_export/HI_CPS-ASEC_state_0209.csv
Data: flu table: FluDrivers
*/

USE FluDrivers;

SELECT year, state_id, name, Total*1000 as pop, Percent_not_covered as pctui, Error_percent_not_covered as pctui_moe
	FROM HI_CPSasec_state 
	WHERE type = "state" and (year >= 2002 AND year <= 2009)
	ORDER BY name, year
;






