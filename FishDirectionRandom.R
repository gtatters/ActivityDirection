########### TABLE OF CONTENTS ############################
# 0.  Libraries - define them all at the top
# 1.  Define Functions
# 2.  Human inputs - manually enter file name as variable f (USER PROVIDES file name)
# 3.  Populate alldata variable from file
# 4a. Centre, Filter/Smooth the x,y detected data
# 4b. Simulate x,y data to check calculations - Remove for actual analysis
# 5.  Clean up the Fish Side Detection
# 6.  Define temperature fish selects and subset by side
# 7.  Movement trajectories using bearing function
# 8.  Plot Fish smooth (x,y) data
# 9.  Plot the trajectory data, polar plot, histograms
#10.  Summaries and final calculations
########### End of TOC ##################################

#0. Libraries required ####
# if missing, type install.packages("packagename") in console
library(plotrix)
library(Thermimage)
library(zoo)
library(signal)
library(pastecs)
library(graphics)
library(fields)
bias.index.c<-NULL
options(warn=-1)

#1. Define Functions ####
bearing<-function(x,y,zmin=0)
  # this function accepts x and y values of detected coordinates for movement
  # and returns a data frame of four variables: magnitude, theta, change, alpha 
  # magnitude and theta define the vectors describing the direction of movement
  # between time points i and i+1.  By necessity, magnitude and theta will be 
  # 1 row shorter than the x,y input coordinates
  # change and alpha represent the change in magnitude and direction in the vector 
  # of movement
  # zmin is the lowest acceptable level of magnitude required to accept the vector
  # of movement of being realisitc (i.e. zmin is the minimum detectable magnitude)
  # http://stackoverflow.com/questions/2571160/calculate-direction-angle-from-two-vectors  
{
  dx<-diff(x)
  dy<-diff(y)
  velocity<-sqrt(dy^2+dx^2)
  # remove really small changes (i.e. zmin=3 units or less)
  dx[which(abs(velocity)<zmin)]<-NA
  dy[which(abs(velocity)<zmin)]<-NA
  velocity[which(abs(velocity)<zmin)]<-NA
  
  theta<-180*atan2(dy,dx)/pi
  # atan2 will calculate angles with respect to the horizontal.
  
  alpha<-diff(theta)
  alpha[which(alpha>180)]<-(-360+alpha[which(alpha>180)])
  alpha[which(alpha<(-180))]<-360+alpha[which(alpha<(-180))]
  # alpha is the instantaneous change in theta with respect to index
  # alternatively called the relative angle or turning angle
  # if the index is in unitary measures of time, alpha becomes degrees/time
  # a +ve alpha corresponds to a left turn, or counter-clockwise turn
  # a -ve alpha corresponds to a right turn, or clock-wise turn
  acceleration<-diff(velocity)
  # change is the magnitude of the relative turn  
  alpha<-c(NA,alpha)
  acceleration<-c(NA,acceleration)
  # assign NA to the first value as a placeholder, so that alpha and change
  # align with the n+1 data points
  result<-data.frame(velocity, theta, acceleration, alpha)
}

addgradient <- function()
  {
  # creates the gradient fill colour ramp for smoothScatter plots
  xm <- get('xm', envir = parent.frame(1))
  ym <- get('ym', envir = parent.frame(1))
  z  <- get('dens', envir = parent.frame(1))
  colramp <- get('colramp', parent.frame(1))
  image.plot(xm,ym,z, col = colramp(256), legend.only = T, add =F)
}

spiral<-function(t=seq(1,100,0.1), size=5, direction="left")
{
  if(direction=="right")
  {
    x<-size*t*sin(t)
    y<-size*t*cos(t)
  }
  else if(direction=="left")
  {
    x<-size*t*cos(t)
    y<-size*t*sin(t)
  }
  out<-cbind(x,y)  
}

#2. Human Inputs - Filename  ####
mainDir<-"~/Documents/R/MyProjects/ActivityDirection/data"
outputDir<-"~/Documents/R/MyProjects/ActivityDirection/output"
#mainDir<-"~/R/GitHub/ActivityDirection-master/data/"  # Folder for student
#outputDir<-"~/R/GitHub/ActivityDirection-master/output/" # Folder for student

dir.create(outputDir, showWarnings = TRUE, recursive = FALSE, mode = "0777")

setwd(mainDir)
l.files<-list.files()
# User input #####
for(f in l.files)
{
Rdatafileout<-paste(sub(".csv", "", f),".Rdata", sep="")
fileout<-paste(sub(".csv", "", f),".csv", sep="")
print(fileout)

#3. Populate alldata data.frame ####
alldata<-read.csv(f, header=TRUE, skip=0, row.names=NULL)
alldata<-na.omit(alldata)
colnames(alldata)<-c("Real", "Elapsed", "X.Pos", "Y.Pos", "Temp.Left",
                     "Temp.Right", "Fish.Side", "Distance.m", "Distance.p",
                     "Velocity.m.s", "Velocity.p.s")

par(mfrow=c(1,1), mar=c(10,5,5,5))
plot(alldata$Temp.Left, main="Click at the time where ramping starts, then <ESC>")
title(sub="Only the data following this time point will be analysed", line=4)
oldpar<-par()

#timeremove<-4677
timeremove<-round(mean(locator(1)$x),0)
# rem out locator line above if running Knit #####
if(timeremove<1) timeremove<-1
cat("The first", timeremove, "data points will be removed")
cat("\n")
#  User input #####
alldata<-alldata[-c(1:timeremove),]
#  remove the first hour (i.e. 3600 s) of data.  

fit<-lm(100*alldata$Distance.m ~ alldata$Distance.p)
b<-fit$coef[1]
m<-fit$coef[2]
# use the data file to ascertain correlation between pixel and cm
colnames(alldata)[5]<-"Temp.Left"
colnames(alldata)[6]<-"Temp.Right"
alldata$X.Pos<-b+m*alldata$X.Pos
alldata$Y.Pos<-b+m*alldata$Y.Pos
alldata<-alldata[-c(8,9,10,11)]
# convert X.Pos and Y.Pos into actual distance units (cm)
alldata$Real<-as.POSIXct(as.character(alldata$Real), tz="", "%H:%M:%S")
alldata$Elapsed<-as.POSIXct(as.character(alldata$Elapsed), tz="", "%H:%M:%S")
Time.diff<-as.integer(alldata$Elapsed-alldata$Elapsed[1])
t<-1:nrow(alldata)
alldata<-data.frame(Time.diff, alldata, row.names=NULL)

#4a. Centre, Filter, and Smooth the x and y data ####
alldata$X.Pos<-alldata$X.Pos-(max(alldata$X.Pos,na.rm=TRUE) + 
                                min(alldata$X.Pos,na.rm=TRUE))/2
alldata$Y.Pos<-alldata$Y.Pos-(max(alldata$Y.Pos,na.rm=TRUE) + 
                                min(alldata$Y.Pos,na.rm=TRUE))/2
half.x<-0
# centre the x data, assuming that the max and min values detected are the extremes

x<-alldata$X.Pos
y<-alldata$Y.Pos
bf<-butter(2, 0.05, type="low")
x.filt1<-filter(bf, x)
y.filt1<-filter(bf, y)
x.filt2<-sgolayfilt(x, 9, 35) 
y.filt2<-sgolayfilt(y, 9, 35)
x.filt<-as.numeric((x.filt1+x.filt2)/2)
y.filt<-as.numeric((y.filt1+y.filt2)/2)
# User input ####
# empirically determined filters - may require adjustment

x.filt<-na.locf(x.filt, na.rm=FALSE)
y.filt<-na.locf(y.filt, na.rm=FALSE)
# replace any NA values with previous non-NA value

# par(mfrow=c(1,2))
# plot(x[10:50], type="l", main="Sample of x values",
#      xlab="Time", ylab="x", col="grey", lty=2)
# lines(x.filt[10:50], type="l", col="blue")
# legend(x="topright", c("Raw", "Filtered"), col=c("grey", "blue"), lty=c(2,1), bty="n")
# plot(y[10:50], type="l", main="Sample of y values",
#      xlab="Time", ylab="y", col="grey", lty=2)
# lines(y.filt[10:50], type="l", col="blue")
# legend(x="topright", c("Raw", "Filtered"), col=c("grey", "blue"), lty=c(2,1), bty="n")
# 10 data points shown for illustrative purposes")

alldata<-data.frame(alldata, x.filt, y.filt, row.names=NULL)

# plot(x[10:60], y[10:60], main="Raw Data (50 Data point Sample)",
#      type='l',  xlim=c(min(x),max(x)), ylim=c(min(y),max(y)))
# plot(x.filt[10:60], y.filt[10:60], main="Filtered Data (50 Data point Sample)",
#      type='l', xlim=c(min(x),max(x)), ylim=c(min(y),max(y)))

#4b. Simulate x,y as a spiral or random #####
rand<-1
# set rand to 1 to run a random sample, #####
# set rand to 0 to run a spiral,
# set rand to any other number to use imported data
maxrepeats<-1
if(rand==1) 
{
  maxrepeats<-10000
}

for(m in 1:maxrepeats)
{
if(rand==1)
{
  #   theta.rand<-runif(nrow(alldata), -1, 1)
  #   mag.rand<-runif(nrow(alldata), 0, 1)
  #   x.rand<-mag.rand*sin(theta.rand)
  #   x.rand<-cumsum(x.rand)
  #   y.rand<-mag.rand*cos(theta.rand)
  #   y.rand<-cumsum(y.rand)
  #   
  index.rand1<-sample(1:nrow(alldata), nrow(alldata))
  #index.rand2<-sample(1:nrow(alldata), nrow(alldata))
  x.rand<-alldata$x.filt[index.rand1]
  y.rand<-alldata$y.filt[index.rand1]
  
  alldata$x.filt<-x.rand
  alldata$y.filt<-y.rand
}
if(rand==0)
{
  spir<-spiral(seq(1,nrow(alldata)), 1, "right")
  alldata$x.filt<-spir[,1]
  alldata$y.filt<-spir[,2]
}


#5.  Clean up Fish.Side ??? detections ####
alldata$Fish.Side<-as.character(alldata$Fish.Side)
q<-which(!alldata$Fish.Side=="???")
if(alldata$Fish.Side[1]=="???") alldata$Fish.Side[1]<-alldata$Fish.Side[q[1]]

for(i in 1:nrow(alldata))
{
  j<-i-1
  #if(alldata$Fish.Side[i]=="???")
  # {
  if(x.filt[i]<half.x)  alldata$Fish.Side[i]<-"LEFT"
  if(x.filt[i]>half.x)  alldata$Fish.Side[i]<-"RIGHT"
  if(x.filt[i]==half.x) alldata$Fish.Side[i]<-alldata$Fish.Side[j]
  #  }
}

alldata$Fish.Side<-as.factor(alldata$Fish.Side)

#6. Define fish temperature ####
slopeleft<-slopeEveryN(alldata$Temp.Left, 30)
sloperight<-slopeEveryN(alldata$Temp.Right, 30)
overallslope<-(slopeleft+sloperight)/2

left.side<-which(alldata$Fish.Side=="LEFT")
right.side<-which(alldata$Fish.Side=="RIGHT")
fish.temp<-1:nrow(alldata) # populate variable fish.temp
fish.temp[left.side]<-alldata$Temp.Left[left.side]
fish.temp[right.side]<-alldata$Temp.Right[right.side]
alldata<-data.frame(alldata,fish.temp, row.names=NULL)

fs<-as.integer(alldata$Fish.Side)-1
# convert Fish.Side character to an integer vector, where Left=0, Right=1
sideswitch<-cbind(1:nrow(alldata), c(0,diff(fs)))
sideswitch<-data.frame(sideswitch)
colnames(sideswitch)<-c("Index","Switch")
# create a indexed matrix that indicates where shuttles occurred
# +1 = moving from left into right (i.e. lower escape)
# - = moving from right into left (i.e. upper escape)
sideswitch<-subset(sideswitch, Switch >0 | Switch<0)
le.index<-sideswitch[which(sideswitch[,2]==1), 1]
ue.index<-sideswitch[which(sideswitch[,2]==-1), 1]
let<-alldata$Temp.Left[le.index]
uet<-alldata$Temp.Right[ue.index]

leshuttles<-data.frame(1:length(let), le.index, alldata$Time.diff[le.index], let)
colnames(leshuttles)<-c("Shuttle.No", "LE.Index", "LE.Time", "LET")
ueshuttles<-data.frame(1:length(uet), ue.index, alldata$Time.diff[ue.index], uet)
colnames(ueshuttles)<-c("Shuttle.No", "UE.Index", "UE.Time", "UET")

#7. Work out movement trajectories using bearing function #####
fish.bearing<-bearing(alldata$x.filt,alldata$y.filt,0)
delta.time<-diff(alldata$Time.diff)
velocity<-fish.bearing[,1]/delta.time
# divide by delta.time in case each row is not 1 sec apart
velocity[is.infinite(velocity)]<-NA
# replace INF values with NA
velocity<-na.locf(velocity, na.rm=FALSE)
# use na.locf to replace NA values with previous entry
theta<-fish.bearing[,2]/delta.time
# divide by alldata$Time.diff in case each row is not 1 sec apart
theta[is.infinite(theta)]<-NA
theta<-na.locf(theta, na.rm=FALSE)
# use na.locf to replace NA values with previous entry
# this will bestow memory to the direction the fish was last moving
acceleration<-abs(fish.bearing[,3]/delta.time)
acceleration[is.infinite(acceleration)]<-NA
alpha<-fish.bearing[,4]/delta.time
alpha[is.infinite(alpha)]<-NA
# divide by alldata$Time.diff in case each row is not 1 sec apart
acceleration<-na.locf(acceleration, na.rm=FALSE)
alpha<-na.locf(alpha, na.rm=FALSE)
# alphas = 0 will mean a stationary animal 
# alphas <> 0 can then be examined for how large or directional they are
alpha[which(alpha>180)]<-(-360+alpha[which(alpha>180)])
alpha[which(alpha<(-180))]<-360+alpha[which(alpha<(-180))]
# convert alpha angles so that 0 to 180 = left, counterclockwise
# and -0 to -180 = right, clockwise
#acceleration<-c(NA, acceleration)
#alpha<-c(NA, alpha)
alldata<-data.frame(alldata[-1,], delta.time, velocity, theta, acceleration, alpha, row.names=NULL)
# remove first row from original dataframe before adding the bearing output
# since the bearing data is based on one less data point

colnames(alldata)<-c("Time.diff", "Real", "Elapsed", "X.Pos", "Y.Pos", "Temp.Left",
                     "Temp.Right", "Fish.Side", "x.filt", "y.filt", "fish.temp", "Delta.time", 
                     "velocity", "theta", "acceleration", "alpha")

#8. Plot Fish smoothed (X,Y) data ####
# par(mfrow=c(1,1), mar=c(5,5,5,5))
# plot(alldata$X.Pos, alldata$Y.Pos, type="l", 
#      main="Fish Positions - Raw", xlab="x", ylab="y")
# plot(alldata$x.filt,alldata$y.filt, type="l", 
#      main="Fish Positions - Smoothed", xlab="x", ylab="y")

#smooth.palette <- colorRampPalette(blues9[-(1:2)])
# smooth.palette <- palette.choose("glowbow")
# smoothScatter(alldata$X.Pos, alldata$Y.Pos, nbin=256, bty="n",
#               main="Density Plot of Fish Positions - Raw", colramp=heat.colors,
#               xlim=c(min(alldata$X.Pos),max(alldata$X.Pos)),
#               ylim=c(min(alldata$Y.Pos),max(alldata$Y.Pos)),
#               postPlotHook=addgradient)
# smoothScatter(alldata$x.filt, alldata$y.filt, nbin=256, bty="n",
#               main="Density Plot of Fish Positions - Smoothed", colramp=heat.colors,
#               xlim=c(min(alldata$X.Pos),max(alldata$X.Pos)),
#               ylim=c(min(alldata$Y.Pos),max(alldata$Y.Pos)),
#               postPlotHook=addgradient)

# Plot the X,Y positions, colour coded by alpha
# par(oldpar)
# par(mfrow=c(1,1), mar=c(5,5,5,5))
# plot(0, 0, type="n", pch=16, xlim=c(min(alldata$x.filt),1.1*max(alldata$x.filt)), 
#      ylim=c(min(y.filt),1.1*max(y.filt)), main="Detected Positions - Filtered",
#      xlab="X", ylab="Y")
# 
# alphaPal <- colorRampPalette(c('red', 'white', 'blue'))
# a.col<-alldata$alpha
# a.col[which(a.col>30)]<-30
# a.col[which(a.col<(-30))]<--30
# alphaCol <- alphaPal(10)[as.numeric(cut(a.col, breaks = 10))]
# points(alldata$x.filt[-1], alldata$y.filt[-1], cex=0.5, pch = 21, col=alphaCol,
#        bg=alphaCol)
# legend(x="topright", c("Right turn", "Left turn"), col=c("red", "blue"), pch=c(1,1), 
#        bty="n")


#9.  Plot the trajectory data ####                                
# par(mfrow=c(1,2))
# polar.plot(velocity, theta, line.col="red", point.col="red", rp.type="s", 
#            start=0, main=("Movement (° vs. cm/s) of Fish"), cex=0.2)
# title(sub="Angle (°) is relative to horizontal = 0°", line=1)
# polar.plot(acceleration[-1], alpha[-1], point.col="red", start=90, rp.type="s",
#            main="Turning Angle (° vs. cm/s/s)", cex=0.2,
#            labels=c(0,20,40,60,80,100,120,140,160,180,-160,-140,-120,-100,
#                     -80,-60,-40,-20))
# title(sub="+° = left, -° = right", line=1)

bin.labels<-seq(-172.5,172.5,15)
bins<-seq(-180,180,15)

par(mfrow=c(1,1))
# freq.alpha<-as.numeric(table(cut(alpha, breaks=bins, labels=bin.labels)))
# polar.plot(100*freq.alpha/sum(freq.alpha), bin.labels, line.col="black", 
#            rp.type="r", start=90, lwd=10, lend=1,
#            main="Distribution of Alphas (% of observed turning angles)",
#            labels=c(0,20,40,60,80,100,120,140,160,180,-160,-140,-120,-100,
#                     -80,-60,-40,-20),
#            mar=c(5,5,5,5))
# title(sub="+° = left, -° = right", line=1)
# 
# par(oldpar)
# par(mfrow=c(2,2), mar=c(5,5,2,5))
# hist(velocity, main="Velocity", xlab="Velocity (cm/s)")
# hist(theta, main="Thetas", xlab="Angle (°)", ylab="# of observations")
# hist(acceleration[-1], main="Acceleration", xlab="Acceleration (cm/s/s)", 
#      ylab="# of observations")
# hist(alpha[-1], main="Alphas", xlab="Angle (°)", ylab="# of observations" )

mean(alpha, na.rm=TRUE)
# par(mfrow=c(1,2), mar=c(5,5,5,5))
# hist(na.omit(alpha), main="Frequency of all alphas", xlab="Turning angle (°)", 
#      ylab="# of observations")
# hist(na.omit(alpha[which(abs(alpha)>20)]), main="Frequency of | alphas | > 20°",
#      xlab="Turning angle (°)", ylab="# of observations")

# par(mfrow=c(1,1))
# freq.alpha20<-as.numeric(table(cut(alpha[which(abs(alpha)>20)], breaks=bins,
#                                    labels=bin.labels)))
# polar.plot(100*freq.alpha20/sum(freq.alpha20), bin.labels, line.col="black", 
#            rp.type="r", start=90, lwd=10, lend=1,
#            main="Distribution of | alphas | > 20° (% of observed turning angles)",
#            labels=c(0,20,40,60,80,100,120,140,160,180,-160,-140,-120,-100,
#                     -80,-60,-40,-20),
#            mar=c(5,5,5,5))
# title(sub="+° = left, -° = right", line=1)


#10. Subset data by side ####
pdr<-subset(alldata, Fish.Side=="RIGHT", row.names=NULL)
pdl<-subset(alldata, Fish.Side=="LEFT", row.names=NULL)

par(oldpar)
# 
# par(mfrow=c(3,1), mar=c(5,5,1,2))
# plot(overallslope, type="l")
# plot(alldata$x.filt, type="l")
# plot(fs, type="l")

# par(oldpar)
# par(mfrow=c(1,1), mar=c(5,5,5,3))
# plot(alldata$Time.diff, alldata$fish.temp, type="l", col="black", main="Behavioural thermoregulation",
#      xlab="Elapsed time (s)", ylab="Selected temperature (°C)")
# points(pdr$Time.diff, pdr$fish.temp, col="red", cex=0.2)
# points(pdl$Time.diff, pdl$fish.temp, col="blue", cex=0.2)
# points(alldata$Time.diff[le.index], let, col="blue")
# points(alldata$Time.diff[ue.index], uet, col="red")


# par(mfrow=c(1,2), mar=c(5,5,5,5))
# hist(let, main="Lower Escape Temperatures", col="blue")
# hist(uet, main="Upper Escape Temperatures", col="red")
stat.desc(let)
stat.desc(uet)
mean(let)
mean(uet)

# par(mfcol=c(2,2), mar=c(5,5,5,5))
# hist(alldata$velocity[le.index], col="blue")
# hist(alldata$velocity[ue.index], col="red")
# 
# hist(alldata$acceleration[le.index], col="blue")
# hist(alldata$acceleration[ue.index], col="red")
# 
# par(mfrow=c(1,1), mar=c(5,5,5,5))
# smoothScatter(alldata$fish.temp, alldata$velocity, nbin=256,
#               colramp=heat.colors,
#               main="Swimming Velocity ~ Selected Temperature",
#               xlab="Selected Temperature",
#               "ylab"="Velocity",
#               postPlotHook=addgradient)
# fit<-lm(velocity~fish.temp, data=alldata)
# abline(coef=fit$coef, xpd=FALSE)


#11. Plot trajectory data based on side ####
# par(mfrow=c(1,1), mar=c(5,5,5,5))
# plot(pdr$Time.diff, pdr$velocity, type="p", col="red", main="Velocity by side",
#      xlab="Elapsed time (s)", ylab="Velocity (cm/s)", cex=0.2)
# points(pdl$Time.diff, pdl$velocity, col="blue", cex=0.2)
# 
# plot(pdr$Time.diff, pdr$acceleration, type="p", col="red", main="Acceleration by side",
#      xlab="Elapsed time (s)", ylab="Acceleration (cm/s/s)", cex=0.2)
# points(pdl$Time.diff, pdl$acceleration, col="blue", cex=0.2)
# 
# plot(pdr$Time.diff, pdr$alpha, type="p", col="red", main="Alpha by side",
#      xlab="Elapsed time (s)", ylab="Turning angle", cex=0.2)
# points(pdl$Time.diff, pdl$alpha, col="blue", cex=0.2)
# 
# par(mfrow=c(1,1), xpd=TRUE)
# polar.plot(pdr$velocity, pdr$theta, point.col="red", rp.type="s", start=0, 
#            main="Movement vectors (° vs. cm/s)", cex=0.2, mar=c(4,4,4,4))
# polar.plot(pdl$velocity, pdl$theta, point.col="blue", cex=0.2, rp.type="s", start=0, 
#            add=TRUE)
# legend(x="topright", c("Right side", "Left side"), col=c("red", "blue"), pch=c(1,1), 
#        bty="n", inset=c(-0.2,0))
# par(mfrow=c(1,1), xpd=TRUE)
# polar.plot(pdr$acceleration[-1], pdr$alpha[-1], point.col="red", start=90, rp.type="s", 
#            main="Turning vectors (° vs. cm/s/s)", mar=c(4,4,4,4), cex=0.2)
# polar.plot(pdl$acceleration[-1], pdl$alpha[-1], point.col="blue", start=90, rp.type="s",
#            add=TRUE, cex=0.2)
# legend(x="topright", c("Right side", "Left side"), col=c("red", "blue"), pch=c(1,1), 
#        bty="n", inset=c(-0.2,0))
# par(oldpar)
# 
# plot(cumsum(na.omit(alldata$alpha)), main="Cumulative ")
# plot(cumsum(na.omit(pdl$alpha)), main="Cumulative ")
# plot(cumsum(na.omit(pdr$alpha)))


#12. Summarise and Output #####
options(scipen=999)

overall.sum<-stat.desc(alldata[,c(11:15)], basic=TRUE, desc=TRUE, norm=FALSE, p=0.95)
overall.sum
right.sum<-stat.desc(pdr[,c(11:15)], basic=TRUE, desc=TRUE, norm=FALSE, p=0.95)
right.sum
left.sum<-stat.desc(pdl[,c(11:15)], basic=TRUE, desc=TRUE, norm=FALSE, p=0.95)
left.sum

# Finalise summary data
Expt.Duration<-max(alldata$Time.diff, na.rm=TRUE) # no. of seconds in the entire experiment
Time.low<-sum(pdl$Delta.time, na.rm=TRUE)    
# no. of seconds spent in the left side
Time.upp<-sum(pdr$Delta.time, na.rm=TRUE)     
# no. of seconds spent in the right side
Duration.low<-Time.low/nrow(leshuttles)
# average duration of time spent in the left side
Duration.upp<-Time.upp/nrow(ueshuttles)
# average duration of time spent in the right side
No.shuttles<-min(nrow(leshuttles),nrow(ueshuttles), na.rm=TRUE)
# number of shuttles overall.  take the min value of lower and upper shuttles
LET.min<-min(leshuttles$LET, na.rm=TRUE)
# minumum lower escape temperature
LET.max<-max(leshuttles$LET, na.rm=TRUE)
# maximum lower escape temperature
LET.avg<-mean(leshuttles$LET, na.rm=TRUE)
# average lower escape temperature
LET.med<-median(leshuttles$LET, na.rm=TRUE)
# median lower escapte temperature (in case distribution is skewed)
UET.min<-min(ueshuttles$UET, na.rm=TRUE)
# minimum upper escape temperature
UET.max<-max(ueshuttles$UET, na.rm=TRUE)
# maximum upper escape temperature
UET.avg<-mean(ueshuttles$UET, na.rm=TRUE)
# average upper escape temperature
UET.med<-median(ueshuttles$UET, na.rm=TRUE)
# median upper escape temperature

#### No filtering
TFish.avg<-mean(alldata$fish.temp, na.rm=TRUE)
# overall average temperature of the fish, based on total time spent at any given temperature
Tfish.med<-median(alldata$fish.temp, na.rm=TRUE)
# overall median temperature of the fish, based on total time spent at any given temperature
Vel.avg<-mean(alldata$velocity, na.rm=TRUE)
# overall average velocity, includes all the 'stationary' periods
Vel.med<-median(alldata$velocity, na.rm=TRUE)
# overall median velocity, includes all the 'stationary' periods
Theta.avg<-mean(alldata$theta, na.rm=TRUE)
# overall average theta
Theta.med<-median(alldata$theta, na.rm=TRUE)
# overall median theta
Accel.avg<-mean(alldata$acceleration, na.rm=TRUE)
# overall average acceleration, includes all the 'stationary' periods
Accel.med<-median(alldata$acceleration, na.rm=TRUE)
# overall median acceleration, includes all the 'stationary' periods
Alpha.avg<-mean(alldata$alpha, na.rm=TRUE)
# overall average alpha, including the 'stationay' periods
Alpha.med<-median(alldata$alpha, na.rm=TRUE)
# overall median alpha, including the 'stationay' periods

filtered.data<-subset(alldata, velocity>Vel.med & abs(alpha)>20)
# create new data according to filter of velocities>median and alphas>20 degrees

counterclockwise<-subset(filtered.data[,11:16], alpha>20)
# subset only those alphas that are >20 degrees into counterclockwise dataframe
clockwise<-subset(filtered.data[,11:16], alpha<(-20))
# subset only those alphas that are < -20 degrees into clockwise dataframe

n.counterclockwise<-nrow(counterclockwise)
n.clockwise<-nrow(clockwise)

Bias.index<-(n.clockwise-n.counterclockwise)/(n.clockwise+n.counterclockwise)
# bias.index is the ratio of the difference in turns to the total counted turns
# -1 = all left, counterclockwise turns
# +1 = all right, clockwise turns
# 0 = no difference in turns
Bias.strength<-abs(mean(clockwise$alpha)+mean(counterclockwise$alpha))

cat(paste("The bias index (-1 = counterclockwise, +1 = clockwise) is: \n",
          round(Bias.index,4)), "\n")
#cat('\n')
cat(paste("The bias strength (|difference in mean ° counterclockwise vs. clockwise|) is: \n",
          round(Bias.strength,4), "\n"))
#cat('\n')


### With filtering 
Vel.avg.f<-mean(filtered.data$velocity, na.rm=TRUE)
Accel.avg.f<-mean(filtered.data$acceleration, na.rm=TRUE)
Theta.avg.f<-mean(filtered.data$theta, na.rm=TRUE)
Alpha.avg.f<-mean(filtered.data$alpha, na.rm=TRUE)

Filename<-f
summaryoutput<-data.frame(Filename, Expt.Duration, Time.low, Time.upp, Duration.low, 
                   Duration.upp, No.shuttles,
                   LET.min, LET.max, LET.avg, LET.med,
                   UET.min, UET.max, UET.avg, UET.med,
                   TFish.avg, Tfish.med, Vel.avg, Vel.med, Theta.avg, Theta.med, 
                   Accel.avg, Accel.med, Alpha.avg, Alpha.med,
                   Vel.avg.f, Accel.avg.f, Theta.avg.f, Alpha.avg.f, 
                   Bias.index, Bias.strength)

setwd(outputDir)

if(file.exists(fileout))
{
  write.table(summaryoutput, fileout, append=TRUE, sep=",", col.names=FALSE, row.names=FALSE)
}
if(!file.exists(fileout))
{
  write.table(summaryoutput, fileout, append=TRUE, sep=",", col.names=TRUE, row.names=FALSE) 
}

# Final File prep will processed data
processed<-list(overall.sum, summaryoutput, pdl, pdr, filtered.data, clockwise,
                counterclockwise, leshuttles, ueshuttles)
names(processed)<-c("overall.sum", "summaryoutput", "pdl", "pdr", "filtered.data", "clockwise",
                    "counterclockwise", "leshuttles", "ueshuttles")
save(processed, file=Rdatafileout)

} # this closes the m loop used for randomisation procedure
} # closes the f loop for file operations