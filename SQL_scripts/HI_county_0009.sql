/*----SQL TEMPLATE----
Author: Elizabeth Lee
Date: 9/29/15
Function: Export county-level percentage uninsured for 2000-09 based on HI_SAHIE_aggregate_ACS, HI_SAHIE_aggregate_CPS, and HI_SAHIE_CPS_2000_Estimates tables.

Command Line: mysql < HI_county_0009.sql | sed 's/\t/,/g' > ../SQL_export/HI_SAHIE_county_0009.csv

Database: FluDrivers
Tables: HI_SAHIE_aggregate_ACS
HI_SAHIE_aggregate_CPS
HI_SAHIE_CPS_2000_Estimates
*/

USE FluDrivers;

SELECT year, county_id, all_ages_percent_uninsured as pctui, all_ages_percent_uninsured_moe as pctui_moe
	FROM HI_SAHIE_CPS_2000_Estimates 
	WHERE type = "county" and (year >= 2000 AND year <= 2009)
UNION ALL
SELECT year, county_id, nui/pop*100 as pctui, nui_moe/pop*100 as pctui_moe
	FROM HI_SAHIE_aggregate_CPS 
	WHERE type = "county" and (year >= 2000 AND year <= 2009)
UNION ALL
SELECT year, county_id, pctui, pctui_moe
	FROM HI_SAHIE_aggregate_ACS 
	WHERE type = "county" and (year >= 2000 AND year <= 2009)
ORDER BY year, county_id
;






