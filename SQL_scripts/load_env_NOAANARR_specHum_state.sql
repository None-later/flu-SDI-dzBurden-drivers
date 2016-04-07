use FluDrivers;

drop table if exists env_NOAANARR_specHum_state;

CREATE TABLE env_NOAANARR_specHum_state(
fips char(2) not null,
year smallint unsigned not null,
daynum smallint unsigned null,
date date,
humidity decimal(10, 8) null
);


LOAD DATA LOCAL INFILE "/home/elee/Dropbox/Sandra Goldlust's Work/Shared_Data/SG_covariate_data/Cleaned_Data/clean_env_noaaNARR0014_state_specHum.csv"
into table env_NOAANARR_specHum_state
FIELDS terminated by ','
OPTIONALLY ENCLOSED BY '"'
ignore 1 lines
(fips, year, daynum, date, humidity);