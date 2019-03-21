#' Export of MIK data from IBTS to HAWG required format (data delivery)
#' 
#' Based on script by J. Devine
#' Modified: March 2019
#' By: Erik Olsen


library(chron)

create.rect.f<-function(data,var1=data$ShootLong,var2=data$ShootLat) {
       data$ices.lon<-ifelse(var1> -1 & var1<0, "E9",
          ifelse(var1>  0 & var1<1,"F0",
          ifelse(var1> 1 & var1<2,"F1",
          ifelse(var1> 2 & var1<3,"F2",
          ifelse(var1> 3 & var1<4,"F3",
          ifelse(var1> 4 & var1<5,"F4",
          ifelse(var1> 5 & var1<6,"F5",
          ifelse(var1> 6 & var1<7,"F6",
          ifelse(var1> 7 & var1<8,"F7",
          ifelse(var1> -3 & var1< -2,"E7",
          ifelse(var1> -2 & var1< -1,"E8",NA)))))))))))
       ##
       data$ices.lat<-ifelse(var2>56 &var2<=56.5,41,
          ifelse(var2>56.5 &var2<=57,42,
          ifelse(var2>57 &var2<=57.5,43,
          ifelse(var2>57.5 &var2<=58,44,
          ifelse(var2>58 &var2<=58.5,45,
          ifelse(var2>58.5 &var2<=59,46,
          ifelse(var2>59 &var2<=59.5,47,
          ifelse(var2>59.5 &var2<=60,48,
          ifelse(var2>60 &var2<=60.5,49,
          ifelse(var2>60.5 &var2<=61,50,
          ifelse(var2>61 &var2<=61.5,51,
          ifelse(var2>61.5 &var2<=62,52,NA))))))))))))
       data$ices.rect<-paste(data$ices.lat,data$ices.lon,sep="")
    }

#' IMPORT MIK DATA
#' ---------------------
setwd("~/ownCloud/Research/iBTS/IBTS Q1 2019/2019102/data/MIK")

#' Need to modify column names to remove -. etc to make it easier for R
x<-read.csv2("MIK MM IBTS Q1 2019 GO Sars final.csv")

#' Add station rectangle to each line
x$statRec<-create.rect.f(x,var1=x$Lon,var2=x$Lat)


#' OUTPUT STATION data
#' ------------------
stn.names<-c('Survey','Country','Ship','Gear','Meshsize','MeshType','StationNumber','HaulID','netopeningArea','Day','Month','Year','hour','minute','Haul',
              'ShootLat','ShootLong','HaulDurationMinutes','HaulDurationSeconds','statrec','Distance.m','AngleOfWire','LengthOfWire','Flowmetertype','FlowRevsInt',
			  'FlowCalInt','ELVolFLAG','SdepthMax','Bdepth')

y<-as.data.frame(matrix(nrow=nrow(x),ncol=length(stn.names)))
names(y)<-stn.names
y$Survey<-'MIK'
y$Country<-'NO'
y$Ship<-'58G2'
y$Gear<-'MRN2'
y$Meshsize<-1600
y$MeshType<-'WoFab'
y$StationNumber<-x$StationNo
temp2 <- mapply(function(x, y) paste0(rep(x, y), collapse = ""), 0, 3 - nchar(y$StationNumber))
#' Update Year to current year!
y$HaulID<-paste(2019,y$Country,'_',paste0(temp2,y$StationNumber),sep='')
y$netopeningArea<-3.1416
y$Day<-as.numeric(ifelse(nchar(x$date)==5,substr(x$date,1,1),substr(x$date,1,2)))
y$Month<-2
#' Update Year to current year!
y$Year<-2019

temp2 <- mapply(function(x, y) paste0(rep(x, y), collapse = ""), 0, 4 - nchar(x$time.hhmm))
x$TID<-paste0(temp2,x$time.hhmm)
y$hour<-as.numeric(format(strptime(x$TID, format="%H%M"), format = "%H"))
y$minute<-as.numeric(format(strptime(x$TID, format="%H%M"), format = "%M"))

y$Haul<-y$StationNumber
y$ShootLat<-x$Lat
y$ShootLong<-ifelse(x$NSEW=='W',-x$Lon,x$Lon)

y$HaulDurationMinutes<-floor(x$duration)
y$HaulDurationSeconds<-as.numeric(substr(x$duration,4,5))
y$HaulDurationSeconds[is.na(y$HaulDurationSeconds)]<-0
y$statrec<-x$statRec
y$Distance.m<-x$dist_towed.Nmi*1852

y$AngleOfWire<-NA
y$LengthOfWire<-NA
y$Flowmetertype<-'MCR'
y$FlowRevsInt<-x$revs
y$FlowCalInt<-3.6236977
y$ELVolFLAG<-'F'
y$SdepthMax<-x$tow_depth
y$Bdepth<-x$w_depth

## error in position in file read in - decimal place in wrong spot
# y$ShootLong[y$StationNumber==98&y$ShootLong<0]<-y$ShootLong[y$StationNumber==98&y$ShootLong<0]*10

write.table(y,"MIK STN data NOR 2019.csv",row.names=F,quote=F,sep=',')



#' OUTPUT CATCH DATA
#' ---------------------------
library(data.table)

temp2 <- mapply(function(x, y) paste0(rep(x, y), collapse = ""), 0, 3 - nchar(x$StationNo))
x$HaulID<-paste(2019,'NO','_',paste0(temp2,x$StationNo),sep='')

x<-x[,c(which(names(x)=='number_caught'):ncol(x))]

summary(x$Not_measured)  ## all were measured - there was no subsampling to account for
x$subsamp.no<-1
x<-x[,-c(2:3,54)]  ## remove no.m2, mean.size and not measured columns
x[is.na(x)]<-0
## Mik Larvae sheet - copied in relavent columns
## scaling factor
# x[is.na(x)]<-0
# for(i in 1:nrow(x)) {x$subsamp.no[i]<-x$number.caught[i] / sum(x[i,c(4:53)],na.rm=T)}
# x[,c(4:53)]<-x[,c(4:53)]*x$subsamp.no



y<-melt(x,id.vars=c('HaulID','number_caught','subsamp.no'))
y[,4]<-as.numeric(substring(y[,4],2,3))     ## NAs created for the 'notmeasured' text
table(y$value)

## don't report them 0s!!!
y<-y[!is.na(y$value),]
y<-y[y$value>0,]
y<-y[order(y$HaulID),]
head(y);nrow(y) # make sure same number as before threw out NAs

## y is the larval sheet
stn.names<-c('HaulID','EggOrLarvae','Species','Length','Number','SubsamplingFactor')
y$EggOrLarvae<-'LV'
y$Species<-'Clupea harengus'
y$Length<-y$variable
y$Number<-y$value
y$SubsamplingFactor<-y$subsamp.no

y<-y[,names(y) %in% stn.names]

write.table(y,"MIK LV data NOR 2019.csv",row.names=F,quote=F,sep=',')


