### Date 12/25/09 - Update 4/30/23
###  This script can be run in its entirety without interaction 

### This code for handling the GSL level data to be first of month records developed for 
### BioWest study and used subsequently in other studies.
##  USGS 10010000 GREAT SALT LAKE AT SALTAIR BOAT HARBOR, UT 
###  is the south arm while
#### USGS 10010100 GREAT SALT LAKE NEAR SALINE, UT
###  is the north arm.
###  The south arm as well as the north arm records started to be daily from 1989-10-01 

# This also outputs data for the full record on the days there were observations

levread.r=function(file="gsl_data_south.txt",skip=31)
{
####reading the data
data<-read.table(file,skip=skip,fill=TRUE)  # 4/23/17 changed skip from 28 to 31 for changed USGS format

#time_in_days gives the original date from the USGS file
#original levels gives the levels from the USGS file
dt<-as.character(data$V3)
level<-as.numeric(as.character(data$V4))  # This required to convert characters, such as Eqp to NA
index=!is.na(level)
list(dt=dt[index],level=level[index])
}

#urlS="http://waterdata.usgs.gov/nwis/dv?cb_72020=on&format=rdb&begin_date=1840-01-01&site_no=10010000&referred_module=sw"
# 4/23/17.  Discovered parameter code had changed
urlS="http://waterdata.usgs.gov/nwis/dv?cb_62614=on&format=rdb&begin_date=1840-01-01&site_no=10010000&referred_module=sw"
name="GSL_south_arm.txt"
download.file(urlS,name,quiet=TRUE)
LevS=levread.r(name,skip=32)

today <- Sys.Date()
nameS=paste("GSL_south_arm_",today,".txt",sep="")
file.copy(name,nameS)  #  Keep dated copy

#urlN="http://waterdata.usgs.gov/nwis/dv?cb_72020=on&format=rdb&begin_date=1900-05-20&site_no=10010100&referred_module=sw"
urlN="http://waterdata.usgs.gov/nwis/dv?cb_62614=on&format=rdb&begin_date=1900-05-20&site_no=10010100&referred_module=sw"
name="GSL_north_arm.txt"
download.file(urlN,name,quiet=TRUE)
LevN=levread.r("GSL_north_arm.txt")
nameN=paste("GSL_north_arm_",today,".txt",sep="")
file.copy(name,nameN)  #  Keep dated copy

enddt=as.Date(LevS$dt[length(LevS$dt)])
enddt=lubridate::rollback(enddt) # to go to the last day of previous month to avoid incomplete month
endy=as.numeric(format(enddt,'%Y'))
endm=as.numeric(format(enddt,'%m'))
if(endm < 9){    # If water year is not complete go back to previous water year
  endy=endy-1
}
enddt=as.Date(paste(endy,"-9-30",sep=""))

# Computing mean levels in S Arm for full record statistics
end_of_wy=seq(as.Date("1847-9-30"),enddt,by="year")
beg_of_wy=seq(as.Date("1847-10-1"),as.Date(paste(endy-1,"-10-1",sep="")),by="year")
lastlevdt=as.Date(paste(endy,"-10-1",sep=""))

Smean=rep(NA,length(end_of_wy)-1)  #  Creating arrays
Dmean=rep(as.Date(LevS$dt[1]),length(end_of_wy)-1)

for(i in 2:length(end_of_wy))
{
indt= (as.Date(LevS$dt)>end_of_wy[i-1] & as.Date(LevS$dt) <=end_of_wy[i])
Smean[i-1]=mean(LevS$level[indt])
Dmean[i-1]=mean(as.Date(LevS$dt[indt]))
}

# Output the year average data
SWyrLevels=data.frame(beg_of_wy, end_of_wy[2:length(end_of_wy)],as.Date(Dmean),Smean)
colnames(SWyrLevels)<-c("Start Date","End Date","Average Date", "Average Level (ft)")
write.table(SWyrLevels,file="SWyrLevels.csv",row.names=FALSE,sep=",")

##  Now output 1 value per month on the first day of the month. Interpolate using linear interpolation
##  Accepting that linear interpolation will be weak during the early record when data is sparse
monthlevwrite.r=function(LevRaw,start="1915-1-1",end="2012-10-1",file="Levels.csv")
{
months=seq(as.Date(start),as.Date(end),by="month")
tt<-data.frame(Date=as.Date(LevRaw$dt),Level_ft=LevRaw$level)
interpolated_ft = approx(tt$Date, tt$Level_ft, xout = months,rule=2)$y
tt<-data.frame(as.Date(months),interpolated_ft,interpolated_ft*0.3048)
colnames(tt)<-c("Date","Level_ft","Level_m")
write.table(tt,file=file,row.names=FALSE,sep=",")
}

daylevwrite.r=function(LevRaw,file="DayLevels.csv")
{
tt<-data.frame(Date=as.Date(LevRaw$dt),Level_ft=LevRaw$level)
write.table(tt,file=file,row.names=FALSE,sep=",")
}

enddt=as.Date(LevS$dt[length(LevS$dt)])
enddt=lubridate::rollback(enddt) # to go to the last day of previous month to avoid incomplete month

monthlevwrite.r(LevS,file="S_Levels.csv",start="1847-11-01",end=enddt)
monthlevwrite.r(LevN,file="N_Levels.csv",start="1966-05-01",end=enddt)

# Daily levels 
daylevwrite.r(LevN,"NorthArmLevelDay.csv")
daylevwrite.r(LevS,"SouthArmLevelDay.csv")


####### Read Level Area & Volume tables
BathFolder="Bathymetry/"
bathtab=read.csv(paste(BathFolder,"GSLM_Bathymetry_noProposedPonds.csv",sep=""))

arean=bathtab$northa+bathtab$northponda
voln=bathtab$northv+bathtab$northpondv
areas=bathtab$southa+bathtab$brba+bathtab$farmba+bathtab$brmbra
vols=bathtab$southv+bathtab$brbv+bathtab$farmbv+bathtab$brmbrv

LAVN=data.frame(level=bathtab$Level*0.3048,area=arean*4047,vol=voln*1233.5256)
LAVS=data.frame(level=bathtab$Level*0.3048,area=areas*4047,vol=vols*1233.5256)
LAVC=data.frame(level=bathtab$Level*0.3048,area=(arean+areas)*4047,vol=(voln+vols)*1233.5256)  # Combined Bathymetry


LAVMAG=LAVS  # S Arm with MagCorp ponds
LAVMAG$area=LAVS$area+bathtab$maga*4047
LAVMAG$vol=LAVS$vol+bathtab$magv*1233.5256

source("GSLFunctions.R")  # run script to create functions

# Get levels data

Slevels=read.csv("S_levels.csv",header=T)
Nlevels=read.csv("N_levels.csv",header=T)

# Prior to 5/1/1966 N level taken as same as S Level
time=as.Date(Slevels$Date)
ntimeind=which(time >= Nlevels$Date[1])

Nlevelsfull=Slevels
Nlevelsfull[ntimeind,]=Nlevels[,]
Nlevels=Nlevelsfull

###Now getting the north areas and volume time series in metric units
north_levels<-Nlevels$Level_m
north_areas<-data.frame(Area_North_sq_m=area_volume.r(LAVN,north_levels)$area)
north_volumes<-data.frame(Volume_North_cu_m=area_volume.r(LAVN,north_levels)$vol)

###Now getting the south areas and volumes time series in metric units
south_levels<-Slevels$Level_m
south_areas<-data.frame(Area_South_sq_m=area_volume.r(LAVS,south_levels)$area)
south_volumes<-data.frame(Volume_South_cu_m=area_volume.r(LAVS,south_levels)$vol)



#  Adjust for magCorp ponds
# Construction started 1970 according to Tripp 2009, but production only really started 1975.  
# 1970 is used for pond closure date because the lake is rising during this period and I wanted a low level
#  to minimize the discontinuity that results when taking differences due to switching bathymetry curves
imag=as.Date(Slevels$Date) < as.Date("1970-1-1") | (as.Date(Slevels$Date) > as.Date("1986-6-1") &  as.Date(Slevels$Date) < as.Date("1994-1-1"))
sapreadj=south_areas$Area_South_sq_m
south_areas$Area_South_sq_m[imag]=area_volume.r(LAVMAG,south_levels[imag])$area

south_volumes_NoMag=south_volumes

south_volumes$Volume_South_cu_m[imag]=area_volume.r(LAVMAG,south_levels[imag])$vol

# combined data frame
combdf=data.frame(Date=Slevels$Date, S_level_ft=Slevels$Level_ft, N_level_ft=Nlevels$Level_ft,S_level_m=Slevels$Level_m,N_level_m=Nlevels$Level_m,
                  S_area_m2=south_areas$Area_South_sq_m,N_area_m2=north_areas$Area_North_sq_m,
                  S_vol_m3=south_volumes$Volume_South_cu_m,N_vol_m3=north_volumes$Volume_North_cu_m,Total_vol_m3=south_volumes$Volume_South_cu_m+north_volumes$Volume_North_cu_m)
# Determine the Natural Lake (no Causeway) level from combined volume. This is the level that a natural lake with N ans S arms
# the same would be.  
natlevel=approx(x=LAVC$vol,y=LAVC$level,xout=combdf$Total_vol_m3[1])

write.table(combdf,file="GSLLevelVol.csv",row.names=FALSE,sep=",")

which(format(as.Date(Slevels$Date),"%m")==10)
indOctDates=which(format(as.Date(Slevels$Date),"%m")==10)
write.table(combdf[indOctDates,],file="GSLLevelVolAnn.csv",row.names=FALSE,sep=",")

# Get areas and volumes for original data on the days of measurements

# Prior to 1966-4-15 use S arm levels for N arm
# Between 1966-4-15 and 1989-10-1 use S arm dates and interpolate to fill in the N arm
# After 1989-10-1 use a daily sequence as the data is daily by and large and fill in the missing using interpolation

t1=as.Date(LevS$dt[which(as.Date(LevS$dt) <= as.Date("1966-04-15"))])
t2=as.Date(LevS$dt[which((as.Date(LevS$dt) > as.Date("1966-04-15")) & (as.Date(LevS$dt) < as.Date("1989-10-01")))])
t3=seq(as.Date("1989-10-01"),as.Date(tail(LevS$dt,1)),by="day")
i1=length(t1)
i2=length(t2)
i3=length(t3)

timesday=c(t1,t2,t3)
Slevelday=rep(NA,length(timesday))
Slevelday[1:i1]=LevS$level[1:i1]
Nlevelday=Slevelday
Slevelday[(i1+1):(i1+i2+i3)]=approx(as.Date(LevS$dt),LevS$level,c(t2,t3))$y
Nlevelday[(i1+1):(i1+i2+i3)]=approx(as.Date(LevN$dt),LevN$level,c(t2,t3))$y

Sarea=area_volume.r(LAVS,Slevelday*.3048)$area
Svol=area_volume.r(LAVS,Slevelday*.3048)$vol
Narea=area_volume.r(LAVN,Nlevelday*.3048)$area
NVol=area_volume.r(LAVN,Nlevelday*.3048)$vol

#Mag Corp Bathymetry adjustments
iodmag=timesday < as.Date("1970-1-1") | (timesday > as.Date("1986-6-1") &  timesday < as.Date("1994-1-1"))

Sarea_premag=Sarea
Svol_premag=Svol
Sarea[iodmag]=area_volume.r(LAVMAG,Slevelday[iodmag]*.3048)$area
Svol[iodmag]=area_volume.r(LAVMAG,LevS$level[iodmag]*.3048)$vol

daydf=data.frame(Date=timesday,Slevel_ft=Slevelday,Slevel_m=Slevelday*.3048,Sarea_m2=Sarea,Svol_m3=Svol,
                 Nlevel_ft=Nlevelday,Nlevel_m=Nlevelday*.3048,Narea_m2=Narea,Nvol_m3=NVol,
                 TotalArea_m2=Sarea+Narea,TotalVol_m3=Svol+NVol)

write.table(daydf,file="GSLLAV.txt",row.names=FALSE)

# Clean up unneeded files
filestodelete=c("N_Levels.csv", "S_Levels.csv", "NorthArmLevelDay.csv","SouthArmLevelDay.csv",
                "SWyrLevels.csv", "levstats.csv")
for(fn in filestodelete){
  if (file.exists(fn)) file.remove(fn)
}



