#' Plotting and analyzing SCANMAR trawl sensor data
#' Generate plots used in trawl calbration (start of survey)
#' By: Erik Olsen
#' v. Feb 2019


#' SETTING SYSTEM TO NORWEGIAN
Sys.setlocale("LC_CTYPE", "no_NO")


#' LIBRARIES
library("ggplot2")
library("RColorBrewer")
library("qqplotr")
library("lattice")


#' SOURCES
setwd("~/ownCloud/Research/iBTS/IBTS Q1 2019/2019102/Cruistools_IBTS")


#' SPECIFY SCANMAR FILES FOR IMPORT
scanmar.files <- list.files("../data/scanmar/")
station.names <- substr(scanmar.files,(nchar(scanmar.files)+1)-13,nchar(scanmar.files)-8)


#' PLOT VERTICAL OPENING, DOOR SPREAD, WING SPREAD - ONE PLOT PER HAUL
for(i in 1:length(scanmar.files)) {
  plotdata <- read.delim2(paste("../data/scanmar/",scanmar.files[i], sep=""), sep=";")
  plotdata$DTG <- strptime(paste(plotdata$Dag, plotdata$Tid), "%d.%m.%Y %H:%M:%S")

    #' Vertical OPENING plot
  topen.plot <- ggplot(plotdata, aes(as.POSIXct(DTG), TEY.1.H)) + geom_point(size=0.3) + geom_hline(yintercept = 3.8, col="blue", linetype=2) + geom_hline(yintercept = 5.1, col="blue", linetype=2) + geom_hline(yintercept = 3, col="green3", linetype=1) + geom_hline(yintercept = 3.5, col="green3", linetype=1) +theme_classic()  + labs(x="time", y="Vertical opening (m)") + coord_cartesian( ylim = c(2,10), expand = TRUE)   + ggtitle(station.names[i])
  
  ggsave(paste("../analyses/calibration_trawl/opening", station.names[i], ".png", sep=""))
  
  #' Door spread plot
  dspread.plot <- ggplot(plotdata, aes(as.POSIXct(DTG), DVTLAM.1.S)) + geom_point(size=0.3) + geom_hline(yintercept = 80, col="blue", linetype=2) + geom_hline(yintercept = 100, col="blue", linetype=2) + geom_hline(yintercept = 100, col="green3", linetype=1) + geom_hline(yintercept = 110, col="green3", linetype=1) +theme_classic()  + labs(x="time", y="Door spread (m)") + coord_cartesian( ylim = c(80,120), expand = TRUE)   + ggtitle(station.names[i])
  
  ggsave(paste("../analyses/calibration_trawl/door_spread", station.names[i], ".png", sep=""))
  
  #'Door spread + Wing spread plot
  dwspread.plot <- ggplot(plotdata, aes(as.POSIXct(DTG), DVTLAM.1.S)) + geom_hline(yintercept = 80, col="blue", linetype=2) + geom_hline(yintercept = 100, col="blue", linetype=2) + geom_hline(yintercept = 100, col="green3", linetype=1) + geom_hline(yintercept = 110, col="green3", linetype=1) +theme_classic()  + labs(x="time", y="Wing spread (m)") + coord_cartesian( ylim = c(10,120), expand = TRUE)  + geom_point(size=0.3) + geom_point(aes(x=as.POSIXct(DTG), y=CVTLAM.1.), data=plotdata, col="RED", size=0.3)  + ggtitle(station.names[i])
  
  ggsave(paste("../analyses/calibration_trawl/wing_door_spread", station.names[i], ".png", sep=""))
}


#' IMPORT ALL SCANMAR DATA 
#' into three variables, 'doors','wings' and 'vopen'
doors <- data.frame( opening=double(),station=integer(), DTG=integer(), No=integer())
wings <- data.frame(station=integer(), opening_x=double(), opening_y=double(), DTG=integer(), No=integer())
vopen <- data.frame( opening=double(),station=integer(), DTG=integer(), No=integer())

for(i in 1:length(scanmar.files)) {
  plotdata <- read.delim2(paste("../data/scanmar/",scanmar.files[i], sep=""), sep=";")
  plotdata$DTG <- strptime(paste(plotdata$Dag, plotdata$Tid), "%d.%m.%Y %H:%M:%S")
  
  d <- as.data.frame(plotdata$DVTLAM.1.S)
  d$station <- station.names[i]
  d$DTG<-plotdata$DTG
  d$No <- c(1:nrow(d))
  colnames(d) <- c("opening", "station", "DTG", "No")
  doors <- rbind(doors, d)
  
  o <-  as.data.frame(plotdata$CVTLAM.1.)
  o$station <- station.names[i]
  o$DTG<-plotdata$DTG
  o$No <- c(1:nrow(o))
  colnames(o) <- c("opening", "station", "DTG", "No")
  wings <- rbind(wings, o)
  
  v <- as.data.frame(plotdata$TEY.1.H)
  v$station <- station.names[i]
  v$DTG<-plotdata$DTG
  v$No <- c(1:nrow(v))
  colnames(v) <- c("opening", "station", "DTG", "No")
  vopen <- rbind(vopen, v)
  
  colnames(doors) <- c("opening", "station", "DTG", "No")
  colnames(vopen) <- c("opening", "station", "DTG", "No")
  colnames(wings) <- c("opening", "station", "DTG", "No")
  
}

#' COMBINED PLOTS
#' 
#'plot all door spread
ds.plot <- ggplot(doors, aes(x=No, y=opening), group=station) + geom_point(aes( colour = factor(station)), size=0.1) + coord_cartesian(ylim=c(75, 135)) +ggtitle("Door spread through haul") + xlab("time (5-sec intervals)") + ylab("Door spread (m)")
ds.plot

ggsave("../analyses/calibration_trawl/door_spread_all_stations.png")

#' plot all door spread - calibration stations
dd <- subset(doors, station == 60001 | station ==60002 | station==60004 | station==60008 | station==60009 | station==60011 | station==60012)
ds.plot <- ggplot(dd, aes(x=No, y=opening), group=station) + geom_point(aes( colour = factor(station)), size=0.1) + geom_hline(yintercept = 80, col="blue", linetype=2) + geom_hline(yintercept = 100, col="blue", linetype=2) + geom_hline(yintercept = 100, col="green3", linetype=1) + geom_hline(yintercept = 110, col="green3", linetype=1)  + facet_wrap(~ station, scales="free") + coord_cartesian(ylim=c(75, 135)) +ggtitle("Door spread through haul - Calibration hauls") + xlab("time (5-sec intervals)") + ylab("Door spread (m)")  +theme_classic()
ds.plot

ggsave("../analyses/calibration_trawl/calibration_door_spread_all_stations.png")


#' plot all wing spread
ws.plot <- ggplot(wings, aes(x=No, y=opening), group=station) + geom_point(aes( colour = factor(station)), size=0.1) + coord_cartesian(ylim=c(15, 25)) +ggtitle("Wings spread through haul") + xlab("time (5-sec intervals)") + ylab("wing spread (m)")
ws.plot

ggsave("../analyses/calibration_trawl/wing_spread_all_stations.png")

#' plot all wing spread - calibration hauls
w1 <- subset(wings, station == 60001 | station ==60002 | station==60004 | station==60008 | station==60009 | station==60011 | station==60012)
ws.plot <- ggplot(w1, aes(x=No, y=opening), group=station) + geom_point(aes( colour = factor(station)), size=0.1) + facet_wrap(~ station, scales="free") + coord_cartesian(ylim=c(15, 25)) +ggtitle("Wings spread through haul") + xlab("time (5-sec intervals)") + ylab("wing spread (m)")+theme_classic()
ws.plot

ggsave("../analyses/calibration_trawl/calibration_wing_spread_all_stations.png")

#' plot all vertical openings
vo.plot <- ggplot(vopen, aes(x=No, y=opening), group=station) + geom_point(aes( colour = factor(station)), size=0.1) + coord_cartesian(ylim=c(1.5, 7)) +ggtitle("Vertical trawl opening through haul") + xlab("time (5-sec intervals)") + ylab("Vertical opening (m)")
vo.plot

ggsave("../analyses/calibration_trawl/vertical_opening_all_stations.png")


#' plot all vertical openings - Calibration - faceted
v1 <- subset(vopen, station == 60001 | station ==60002 | station==60004 | station==60008 | station==60009 | station==60011 | station==60012)
vo.plot <- ggplot(v1, aes(x=No, y=opening), group=station) + geom_point(aes( colour = factor(station)), size=0.1) + geom_hline(yintercept = 3.8, col="blue", linetype=2) + geom_hline(yintercept = 5.1, col="blue", linetype=2) + geom_hline(yintercept = 3, col="green3", linetype=1) + geom_hline(yintercept = 3.5, col="green3", linetype=1) + facet_wrap(~ station, scales="free") + coord_cartesian(ylim=c(1.5, 7)) +ggtitle("Vertical trawl opening through haul") + xlab("time (5-sec intervals)") + ylab("Vertical opening (m)")+theme_classic()
vo.plot

ggsave("../analyses/calibration_trawl/calibration_vertical_opening_all_stations.png")

#' plot all vertical openings - Calibration vs trawl with +14 floats- faceted
v1 <- subset(vopen, station==60008 | station==60009 | station==60011 | station==60012|station==60015|station==60016|station==60017)

s1<-data.frame(station=levels(as.factor(v1$station)), N_Floats=c("70", "70", "70", "70", "84", "84", "84"))

v1$N_Floats <- as.factor(s1$N_Floats[match(v1$station, s1$station)])


vo.plot <- ggplot(v1, aes(x=No, y=opening), group=type) + geom_point(aes( colour = N_Floats), size=0.1) + geom_hline(yintercept = 3.8, col="blue", linetype=2) + geom_hline(yintercept = 5.1, col="blue", linetype=2) + geom_hline(yintercept = 3, col="green3", linetype=1) + geom_hline(yintercept = 3.5, col="green3", linetype=1) + facet_wrap(~ station, scales="free") + coord_cartesian(ylim=c(1.5, 7)) +ggtitle("Vertical trawl opening") + xlab("time (5-sec intervals)") + ylab("Vertical opening (m)")+theme_classic() + scale_fill_discrete(name="Number of floats")
vo.plot


ggsave("../analyses/calibration_trawl/vopen_cali_vs_more_floats_.png")



#' Q-Q PLOTS
#' Quantile-Quantile (Q-Q) plots of door-spread, wing-spread and vertical opening


#' DOOR SPREAD

#' Remove NA's
dd <- as.data.frame(na.omit(doors))

#'faceted qqlotr door spread 
dd1 <- subset(subset(dd, opening>50), opening<130)
gg <- ggplot(data = dd1, mapping = aes(sample = opening,  fill = factor(station))) + stat_qq_band() + stat_qq_line() + stat_qq_point(size=0.2) + facet_wrap(~ station, scales="free") +  labs(x = "Theoretical Quantiles", y = "Door spread (observed)") + ggtitle("Door Opening , Q-Q plots")
gg

ggsave("../analyses/calibration_trawl/door_spread_facet_qq.png")

#' faceted qqlotr door spread  - calibration stations
dd2 <- rbind(subset(dd1, station==60001), subset(dd1, station==60002), subset(dd1, station==60004), subset(dd1, station==60008), subset(dd1, station==60009), subset(dd1, station==60011), subset(dd1, station==60012))
gg <- ggplot(data = dd2, mapping = aes(sample = opening,  fill = factor(station))) + stat_qq_band() + stat_qq_line() + stat_qq_point(size=0.2) + facet_wrap(~ station, scales="free") +  labs(x = "Theoretical Quantiles", y = "Door spread (observed)") + ggtitle("Door Spread - Calibration tows, Q-Q plots")
gg

ggsave("../analyses/calibration_trawl/calibration_door_spread_facet_qq.png")


#' WING SPREAD

#' Remove NA's
ww <- as.data.frame(na.omit(wings))

#' wing spread qq plot
quant.wings <- ggplot(ww) + stat_qq(aes(sample = opening, colour = factor(station)), size=0.3) +  scale_colour_hue("clarity")  + labs(colour = "GOV Station") + ggtitle("Wing spread") + coord_cartesian( ylim = c(15,30), expand = TRUE)
quant.wings
ggsave("../analyses/calibration_trawl/wing_spread_qq.png")

#' faceted qqlotr wing spread
gg <- ggplot(data = ww, mapping = aes(sample = opening,  fill = factor(station))) + stat_qq_band() + stat_qq_line() + stat_qq_point(size=0.2) + facet_wrap(~ station, scales="free") +  labs(x = "Theoretical Quantiles", y = "Wing spread (observed)") + ggtitle("Wing Spread, Q-Q plots")
gg

ggsave("../analyses/calibration_trawl/wing_spread_facet_qq.png")

#' faceted qqlotr wing spread - calibration tows
ww0 <- rbind(subset(ww, station==60001), subset(vo, station==60002),  subset(dd1, station==60004), subset(dd1, station==60008), subset(dd1, station==60009), subset(dd1, station==60011), subset(dd1, station==60012))
gg <- ggplot(data = ww0, mapping = aes(sample = opening,  fill = factor(station))) + stat_qq_band() + stat_qq_line() + stat_qq_point(size=0.2) + facet_wrap(~ station, scales="free") +  labs(x = "Theoretical Quantiles", y = "Wing spread (observed)") + ggtitle("Wing Spread, Calibration tows, Q-Q plots ")
gg

ggsave("../analyses/calibration_trawl/calibration_wing_spread_facet_qq.png")


#' VERTICAL OPENING

#' remove NA's and subset to opening between 2 - 6m 
vo <- as.data.frame(na.omit(vopen))
vo <- subset(vo, opening>2)
vo <- subset(vo, opening<6)

#' vertical opening Q-Q plot
qq_vopen.plot <- ggplot(vo) + stat_qq(aes(sample = opening, colour = factor(station)), size=0.3) +  scale_colour_hue("clarity")  + labs(colour = "GOV Station") + ggtitle("Vertical trawl opening") + coord_cartesian( ylim = c(2,7), expand = TRUE)
qq_vopen.plot
ggsave("../analyses/calibration_trawl/vertical_opening_qq.png")

#' faceted qqlotr vertical opening
qq_vopen.plot <- ggplot(data = vo, mapping = aes(sample = opening,  fill = factor(station))) + stat_qq_band() + stat_qq_line() + stat_qq_point(size=0.2) + facet_wrap(~ station, scales="free") +  labs(x = "Theoretical Quantiles", y = "Vertical opening (observed)") + ggtitle("Vertical trawl opening, Q-Q plots")
qq_vopen.plot

ggsave("../analyses/calibration_trawl/vertical_trawl_opening_facet_qq.png")

#' faceted qqlotr vertical opening - calibration tows
vo2 <- rbind(subset(vo, station==60001), subset(vo, station==60002),  subset(dd1, station==60004), subset(dd1, station==60008), subset(dd1, station==60009), subset(dd1, station==60011), subset(dd1, station==60012))
qq_vopen.plot <- ggplot(data = vo2, mapping = aes(sample = opening,  fill = factor(station))) + stat_qq_band() + stat_qq_line() + stat_qq_point(size=0.2) + facet_wrap(~ station, scales="free") +  labs(x = "Theoretical Quantiles", y = "Vertical opening (observed)") + ggtitle("Vertical trawl opening - Calibration tows, Q-Q plots")
qq_vopen.plot

ggsave("../analyses/calibration_trawl/calibration_vertical_trawl_opening_facet_qq.png")


