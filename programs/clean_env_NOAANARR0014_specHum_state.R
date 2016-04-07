# author: Elizabeth Lee, adapted from code from Cecile Viboud
# purpose: clean gridded netCDF of specific humidity from NOAA NARR for loading into FluDrivers mysql database
# data scale: state level, daily from 2000 to 2014
# source files: shum.2m.yyyy.nc files, downloaded from http://www.esrl.noaa.gov/psd/data/gridded/data.narr.monolevel.html
# 4/17/16

library(ncdf4)
library(fields)
library(chron)
require(dplyr); require(tidyr); require(readr)

setwd(dirname(sys.frame(1)$ofile))
setwd("../")
state.coord <- read_csv("StatePopCenter.csv", col_types = "cciddi") %>%
  mutate(fips = substr.Right(paste0("0", STATEFP), 2))
state.coord <- as.data.frame(state.coord)

#################################
# open 2010 nc file as an example
tmp2010.nc = nc_open("shum.2m.2010.nc")
print(tmp2010.nc) # allows you to view the documentation about the file
summary(tmp2010.nc)

# assign varsize, get lat and lon matrixes
varsize <- tmp2010.nc$var[['shum']]$varsize
lat <- ncvar_get(nc=tmp2010.nc, varid="lat")
lon <- ncvar_get(nc=tmp2010.nc, varid="lon")

#################################
# custom functions

# i,j refers to the grid cell location and the grid cells are locally homogeneous (even though the earth is a sphere)
# both i&j information are needed to get both latitude and longitude coordinates
# each grid cell is then linked to the closest state via lat/lon coordinates
# It's not clear where/how the grid cells are being converted to lat/lon and stored in the lat and lon matrixes
getidxfromcoord= function(city.lat,city.lon)
{
diff=matrix(NA,varsize[1],varsize[2])
for (i in (1:varsize[1])){
	for (j in (1:varsize[2])){
		diff.lat=(city.lat-lat[i,j])^2	
		diff.lon=(city.lon-lon[i,j])^2
		diff[i,j]=diff.lat+diff.lon}}
idx=which(diff==min(diff),arr.ind=T)
return(idx)
}

getyear= function(year)
{
  data.nc = nc_open(paste("shum.2m.",year,".nc",sep=""))
  varsize = data.nc$var[['shum']]$varsize 
  
  data.mat=matrix(NA,nrow=dim(state.coord)[1],ncol=data.nc$var[['shum']]$varsize[3])
  for (i in (1:dim(state.coord)[1]))
  {start = c(state.coord$row[i], state.coord$col[i], 1) 
  data.mat[i,] = ncvar_get(nc=data.nc,varid="shum",start,count)
  }
  
  return(data.mat)
}

#################################
# for each state, get the index of the grid lat/lon that is closest to the inputted state's lat/lon 
idx=matrix(NA,nrow=dim(state.coord)[1],ncol=2)
for (i in (1:dim(state.coord)[1]))
	{idx[i,]=getidxfromcoord(state.coord$LATITUDE[i] ,state.coord$LONGITUDE[i])}

# for each state, get the lat/lon coordinates of the grid cell that best match the reported lat/lons from StatePopCenter.csv
state.coord$row=idx[,1]
state.coord$col=idx[,2]
for (i in (1:dim(state.coord)[1]))
	{state.coord$est.lat[i]=lat[state.coord$row[i],state.coord$col[i]]
	state.coord$est.lon[i]=lon[state.coord$row[i],state.coord$col[i]]
	}


#################################
## plot 2010 data as a check ##

# count is a vector of integers indicating the count of vectors to read along each dimension (X-Y-Z-T)
# -1 means that all entries along that dimension should be read
count = c(1, 1, -1)

shum10.mat=matrix(NA,nrow=dim(state.coord)[1],ncol=tmp2010.nc$var[['shum']]$varsize[3]) # ncol is the time dimension in days
for (i in (1:dim(state.coord)[1]))
	{start = c(state.coord$row[i], state.coord$col[i], 1) 
	shum10.mat[i,] = ncvar_get(nc=tmp2010.nc,varid="shum",start,count)
	}

date=seq(from=2010,to=2010.999,length.out=tmp2010.nc$var[['shum']]$varsize[3])

plot(date,shum10.mat[10,],col="red",type="l")
lines(date,shum10.mat[12,],col="green")
lines(date,shum10.mat[5,],col="purple")
lines(date,shum10.mat[35,],col="blue")
lines(date,shum10.mat[33,],col="black")
legend(x="topleft",legend=c("FL","HI","CA","ND","NY"),col=c("red","green","purple","blue","black"),lty=1,lwd=2)

#################################
# export and plot all data

full.mat <- matrix(NA, nrow = dim(state.coord)[1], ncol = 1)
for (y in (2000:2014)){
  tmp.mat <- as.data.frame(getyear(y))
  names(tmp.mat) <- paste0("y", y, "_d", 1:(dim(tmp.mat)[2]))
  full.mat <- cbind(full.mat, tmp.mat)
	print(paste(y))
	}

datee=seq(from=2000,to=2014.9999,length.out=dim(full.mat)[2])
plot(datee,full.mat[10,],col="red",type="l")
lines(datee,full.mat[12,],col="green")
lines(datee,full.mat[5,],col="purple")
lines(datee,full.mat[35,],col="blue")
lines(datee,full.mat[33,],col="black")
legend(x="topleft",legend=c("FL","HI","CA","ND","NY"),col=c("red","green","purple","blue","black"),lty=1,lwd=2)
title("Daily shum, 2000-2014")

names(full.mat)[1] <- "fips"
full.mat[,1] <- as.character(state.coord$fips)

full.mat2 <- tbl_df(as.data.frame(full.mat)) %>%
  gather(time, humidity, y2000_d1:y2014_d365, convert=TRUE) %>%
  mutate(year = as.numeric(substring(time, 2, 5))) %>%
  mutate(daynum = as.numeric(substring(time, 8, nchar(time)))) %>%
  mutate(date = as.Date(strptime(paste(year, daynum), format = "%Y %j"))) %>%
  select(fips, year, daynum, date, humidity)

## write to file
setwd("../../../../../Sandra Goldlust's Work/Shared_Data/SG_covariate_data/Cleaned_Data")
write_csv(full.mat2,"clean_env_noaaNARR0014_state_specHum.csv")
