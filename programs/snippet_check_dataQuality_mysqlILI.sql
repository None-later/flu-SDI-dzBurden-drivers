/*----SQL TEMPLATE----
Author: Elizabeth Lee
Date: July 13, 2016
Function: Perform checks to interpret the meaning of missing ILI data and missing pop data
1. We hypothesize that zip3-weeks with missing data for ILI had no claims data at all, and that zip3-weeks with zeros for ILI had some medical claims but just none for ILI.

Command Line: run in mysql

Database: sdi
Tables: flu
*/

/* 
Q: Are there entries where any diagnosis is zero?
A: No, this is an empty set.
*/
select * from flu where ANY_DIAG_VISIT_CT = 0;

/* 
Q: Any patterns among zip3-weeks where ILI = 0?
A: There were many rows with 0 ILI. Break down the question further.
*/
select * from flu where ILI_m = 0 limit 500;

/* 
Q: Any patterns among zip3-weeks where ILI & RSV = 0?
A: No, this is an empty set.
*/
select * from flu where ILI_m = 0 and RSV_m = 0;

/* 
Q: Examine rows where pop = 0
A: It appears that pop=0 for rows where either ILI or RSV are 0
*/
select * from flu where POPSTAT=0;


/* 
Q: Is pop = 0 for every instance where ILI = 0?
A: No
*/
select * from flu where ILI_m = 0 AND POPSTAT > 0;

/* 
Q: Is pop = 0 for every instance where RSV = 0?
A: No
*/
select * from flu where RSV_m = 0 AND POPSTAT > 0;

/* 
Q: Is ILI or RSV = 0 for all instance of pop = 0?
A: No, there are 443 instances where pop = 0 but ILI and RSV both have data. This spans multiple zip3s: 007, 008, 098, 311, 332,398, 753, 772, 851, 963, 969
*/
select * from flu where (ILI_m != 0 and RSV_m != 0) AND POPSTAT = 0;

/* 
Q: For how many rows is ILI > 0 and pop = 0?
A: 17,948 rows total, 2792 rows for total agegroup and service place
*/
select * from flu where ILI_m != 0 AND POPSTAT = 0 AND AGEGROUP = "TOTAL" AND SERVICE_PLACE = "TOTAL";

/* 
Q: How many distinct zip3s are affected by 0 popstat?
A: 52; includes 202 and 942
*/
select distinct PATIENT_ZIP3 from flu where ILI_m != 0 AND POPSTAT = 0 and SERVICE_PLACE = "TOTAL" and AGEGROUP = "TOTAL";
 List of affected zip3s
| 002          |
| 003          |
| 004          |
| 005          |
| 006          |
| 007          |
| 008          |
| 009          |
| 055          |
| 090          |
| 091          |
| 092          |
| 093          |
| 094          |
| 095          |
| 096          |
| 097          |
| 098          |
| 192          |
| 202          |
| 203          |
| 204          |
| 205          |
| 311          |
| 332          |
| 340          |
| 375          |
| 398          |
| 399          |
| 419          |
| 459          |
| 509          |
| 555          |
| 569          |
| 649          |
| 733          |
| 753          |
| 772          |
| 842          |
| 851          |
| 872          |
| 885          |
| 889          |
| 901          |
| 938          |
| 942          |
| 962          |
| 963          |
| 964          |
| 965          |
| 966          |
| 969          |
+--------------+


/* 
Q: Explore "202" time series
A: Data for entire period are sparse (only 31 out of all weeks had non-zero ILI). This zip3 only represents federal buildings in Washington DC.
*/
select * from flu where PATIENT_ZIP3 = '202' and SERVICE_PLACE = "TOTAL" and AGEGROUP = "TOTAL";

/* 
Q: Explore "942" time series
A: Data for entire period are sparse (only 54 out of all weeks had non-zero ILI). This zip3 is supposed to be in Sacramento, CA but does not appear to be associated with physical boundaries.
*/
select * from flu where PATIENT_ZIP3 = '942' and SERVICE_PLACE = "TOTAL" and AGEGROUP = "TOTAL";

/*
Bottom line: It appears that zip3s with no pop data aren't really locations with populations. They include places like military bases, federal buildings, and administrative areas. They do not appear to be associated with ZCTA physical spaces and it's likely that we won't be able to find population data for them through any source.
*/


/* 
Q: Cross-check a few zip3 populations in the IMS data
A: These seem to be in line with the 2010 Census population by zipcode data cleaned/downloaded at Splitwise Blog (see Census/Source_Data/'2010+Census+Population+By+Zipcode+(ZCTA).csv')
*/
select distinct PATIENT_ZIP3, POPSTAT from flu where (PATIENT_ZIP3 = '945' or PATIENT_ZIP3 = '200') and SERVICE_PLACE = "TOTAL" and AGEGROUP = "TOTAL";
