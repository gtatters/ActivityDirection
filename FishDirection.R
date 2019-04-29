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
# 8.  Plot Fish smooth (x,y) dataa
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
library(plyr)
source("https://www.dropbox.com/s/168hsu4xuus0o1d/CommonFunctions.R?dl=1")
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

alphacumulate<-function(alphas, events){
  alphas[is.na(alphas)]<-0
  cumulate<-NULL
  z<-max(events)
  
  for(i in 1:z){
    tmp<-cumsum(alphas[events==i])
    cumulate<-c(cumulate, tmp)
  }
return(cumulate)
}


#2. Human Inputs - Filename  ####
#mainDir<-"~/Dropbox/R/MyProjects/ActivityDirection/data"
#outputDir<-"~/Dropbox/R/MyProjects/ActivityDirection/output"

mainDir<-"~/Desktop/Justin/data/"  # Folder for student
outputDir<-"~/Desktop/Justin/output/" # Folder for student

#mainDir<-"~/Desktop/Justin/Revised Shuttlebox Results/21C-acc September 2016 Shuttlebox/Ramping Files"

dir.create(outputDir, showWarnings = F, recursive = FALSE, mode = "0777")
setwd(mainDir)

l.files<-list.files()
l.files
# User input ###
f<-l.files[1] 
f

#for(f in l.files)
#{
setwd(mainDir)
Rdatafileout<-paste(sub(".csv", "", f),".rds", sep="")
timesummaryout<-paste(sub(".csv", "", f),"_by_time.csv", sep="")
shuttlesummaryout<-paste(sub(".csv", "", f),"_by_shuttle.csv", sep="")
fileout<-"Output.csv"


#3. Populate alldata data.frame ####
alldata<-read.csv(f, header=TRUE, skip=0, row.names=NULL)
alldata<-na.omit(alldata)
colnames(alldata)<-c("Real", "Elapsed", "X.Pos", "Y.Pos", "Area", "Temp.Left",
                     "Temp.Right", "Fish.Side", "Distance.m", "Distance.p",
                     "Velocity.m.s", "Velocity.p.s")

# Look at Area size. If Area=0, problem with detection
ind<-which(alldata$Area==0)
# replace the erroneous detections with the previous row.  
alldata$X.Pos[ind]<-alldata$X.Pos[(ind-1)]
alldata$Y.Pos[ind]<-alldata$Y.Pos[(ind-1)]

alldata$X.Pos[which(alldata$X.Pos<0)]<-NA
alldata$Y.Pos[which(alldata$Y.Pos<0)]<-NA

alldata$X.Pos<-na.fill(alldata$X.Pos, "extend")
alldata$Y.Pos<-na.fill(alldata$Y.Pos, "extend")

par(mfrow=c(1,1), mar=c(10,5,5,5))
plot(alldata$Temp.Left, main="Click at the time where ramping starts, then <ESC>")
title(sub="Only the data following this time point will be analysed", line=4)
oldpar<-par()

timeremove<-0
timeremove<-round(mean(locator(1)$x),0)
# rem out line above with timeremove and locator if running Knit #####

if(timeremove<1) timeremove<-1
cat("The first", timeremove, "data points will be removed")
cat("\n")
#  User input #####
alldata<-alldata[-c(1:timeremove),]
#  remove the first hour (i.e. 3600 s) of data.  

#4a. Centre, Filter, and Smooth the x and y data ####
# alldata$X.Pos<-alldata$X.Pos-(max(alldata$X.Pos,na.rm=TRUE) + 
#                               (min(alldata$X.Pos,na.rm=TRUE))/2
# alldata$Y.Pos<-alldata$Y.Pos-(max(alldata$Y.Pos,na.rm=TRUE) + 
#                                 min(alldata$Y.Pos,na.rm=TRUE))/2
alldata$X.Pos<-alldata$X.Pos-160
alldata$Y.Pos<-alldata$Y.Pos-120
half.x<-0
# centre the x data, assuming that the max and min values detected are the extremes

# Convert the X.Pos into true distance (cm)
fit<-lm(100*Distance.m ~ Distance.p, data=alldata)
b<-fit$coef[1]
b<-0
m<-fit$coef[2]
# use the data file to ascertain correlation between pixel and cm
alldata$X.Pos<-b+m*alldata$X.Pos
alldata$Y.Pos<-b+m*alldata$Y.Pos
alldata<-alldata[c("Real", "Elapsed", "X.Pos", "Y.Pos", "Area", "Temp.Left",
                   "Temp.Right", "Fish.Side")]
# convert X.Pos and Y.Pos into actual distance units (cm)
alldata$Real<-as.POSIXct(as.character(alldata$Real), tz="", "%H:%M:%S")
alldata$Elapsed<-as.POSIXct(as.character(alldata$Elapsed), tz="", "%H:%M:%S")
Time.diff<-as.integer(alldata$Elapsed-alldata$Elapsed[1])
t<-1:nrow(alldata)
alldata<-data.frame(Time.diff, alldata, row.names=NULL)


x<-alldata$X.Pos
y<-alldata$Y.Pos
bf<-butter(2, 0.2, type="low")
x.filt1<-filter(bf, x)
y.filt1<-filter(bf, y)
x.filt2<-sgolayfilt(x, 9, 55) 
y.filt2<-sgolayfilt(y, 9, 55)
x.filt<-as.numeric((x.filt1+x.filt2)/2)
y.filt<-as.numeric((y.filt1+y.filt2)/2)
# User input ####
# empirically determined filters - may require adjustment

x.filt<-na.locf(x.filt, na.rm=FALSE)
y.filt<-na.locf(y.filt, na.rm=FALSE)
# replace any NA values with previous non-NA value

par(mfrow=c(1,2))
plot(x[10:100], type="l", main="Sample of x values",
     xlab="Time", ylab="x", col="grey", lty=2)
lines(x.filt[10:100], type="l", col="blue")
legend(x="topright", c("Raw", "Filtered"), col=c("grey", "blue"), lty=c(2,1), bty="n")
plot(y[10:100], type="l", main="Sample of y values",
     xlab="Time", ylab="y", col="grey", lty=2)
lines(y.filt[10:100], type="l", col="blue")
legend(x="topright", c("Raw", "Filtered"), col=c("grey", "blue"), lty=c(2,1), bty="n")
# 10 data points shown for illustrative purposes")

alldata<-data.frame(alldata, x.filt, y.filt, row.names=NULL)

plot(x, y, main="Raw Data",
     type='l',  xlim=c(min(x),max(x)), ylim=c(min(y),max(y)))
plot(x.filt, y.filt, main="Filtered Data",
     type='l', xlim=c(min(x),max(x)), ylim=c(min(y),max(y)))

#4b. Simulate x,y as a spiral or random #####
rand<-2
# set rand to 1 to run a random sample, #####
# set rand to 0 to run a spiral,
# set rand to any other number to use imported data
maxrepeats<-1
if(rand==1) 
{
  maxrepeats<-10000
}

#for(m in 1:maxrepeats)
#{

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
alldata<-data.frame(alldata, fish.temp, row.names=NULL)

fs<-as.integer(alldata$Fish.Side)-1
# convert Fish.Side character to an integer vector, where Left=0, Right=1
sideswitch<-cbind(1:nrow(alldata), c(0,diff(fs)))
sideswitch<-data.frame(sideswitch)
colnames(sideswitch)<-c("Index","Switch")
# create a indexed matrix that indicates where shuttles occurred
# +1 = moving from left into right (i.e. lower escape)
# - = moving from right into left (i.e. upper escape)
sideswitch<-subset(sideswitch, Switch >0 | Switch<0)
sideswitch$Event<-1:nrow(sideswitch)

le.index<-sideswitch[which(sideswitch[,2]==1), 1]
ue.index<-sideswitch[which(sideswitch[,2]==-1), 1]
let<-alldata$Temp.Left[le.index]
uet<-alldata$Temp.Right[ue.index]
leshutNo<-1:length(le.index)
ueshutNo<-1:length(ue.index)

alldata$Event<-NA
alldata$Event[sideswitch$Index]<-sideswitch$Event
if(is.na(alldata$Event[1])){
  alldata$Event<-alldata$Event+1
  alldata$Event[1]<-1
}
alldata$Event<-na.locf(alldata$Event)


#7. Work out movement trajectories using bearing function #####
fish.bearing<-bearing(alldata$x.filt, alldata$y.filt, 0)
#fish.bearing<-bearing(alldata$X.Pos,alldata$Y.Pos, 0)
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

colnames(alldata)<-c("Time.diff", "Real", "Elapsed", "X.Pos", "Y.Pos", "Area", "Temp.Left",
                     "Temp.Right", "Fish.Side", "x.filt", "y.filt", "fish.temp", "Event", 
                     "Delta.time", "velocity", "theta", "acceleration", "alpha")

alldata$distance<-alldata$velocity*alldata$Delta.time



localmax<-function(dat, ind, rng=9){
  startind<-ind-(rng-1)/2
  endind<-ind+(rng-1)/2
  allindex<-unlist(Map(':', startind, endind))
  z<-matrix(dat[allindex], nrow=rng)
  lmax<-apply(z, 2, max)
  return(lmax) # returns the local max value 
}

# Extract velocity and acceleration values coinciding with each shuttle.  extract the maximum value to 
# estimate burst values and to avoid stationary data points
vellet<-localmax(alldata$velocity, ind=le.index, rng=9)
veluet<-localmax(alldata$velocity, ind=ue.index, rng=9)
acclet<-localmax(alldata$acceleration, ind=le.index, rng=9)
accuet<-localmax(alldata$acceleration, ind=ue.index, rng=9)


# create shuttle only data frames for eventual export
leshuttles<-data.frame(1:length(let), le.index, alldata$Time.diff[le.index], let, vellet, acclet)
colnames(leshuttles)<-c("Shuttle.No", "LE.Index", "LE.Time", "LET", "Veloc.LET", "Accel.LET")
ueshuttles<-data.frame(1:length(uet), ue.index, alldata$Time.diff[ue.index], uet, veluet, accuet)
colnames(ueshuttles)<-c("Shuttle.No", "UE.Index", "UE.Time", "UET", "Veloc.UET", "Accel.UET")
allshuttles<-merge(leshuttles, ueshuttles)

#plot(allshuttles$Veloc.LET~allshuttles$LET)
#plot(allshuttles$Veloc.UET~allshuttles$UET)



#8. Plot Fish smoothed (X,Y) data ####
par(mfrow=c(1,1), mar=c(5,5,5,5))
plot(alldata$X.Pos, alldata$Y.Pos, type="l", 
     main="Fish Positions - Raw", xlab="x", ylab="y")
plot(alldata$x.filt,alldata$y.filt, type="l", 
     main="Fish Positions - Smoothed", xlab="x", ylab="y")

#smooth.palette <- colorRampPalette(blues9[-(1:2)])
smooth.palette <- palette.choose("glowbow")
smoothScatter(alldata$X.Pos, alldata$Y.Pos, nbin=256, bty="n",
              main="Density Plot of Fish Positions - Raw", colramp=heat.colors,
              xlim=c(min(alldata$X.Pos),max(alldata$X.Pos)),
              ylim=c(min(alldata$Y.Pos),max(alldata$Y.Pos)),
              postPlotHook=addgradient)
smoothScatter(alldata$x.filt, alldata$y.filt, nbin=256, bty="n",
              main="Density Plot of Fish Positions - Smoothed", colramp=heat.colors,
              xlim=c(min(alldata$X.Pos),max(alldata$X.Pos)),
              ylim=c(min(alldata$Y.Pos),max(alldata$Y.Pos)),
              postPlotHook=addgradient)

# Plot the X,Y positions, colour coded by alpha
par(oldpar)
par(mfrow=c(1,1), mar=c(5,5,5,5))
plot(0, 0, type="n", pch=16, xlim=c(min(alldata$x.filt),1.1*max(alldata$x.filt)), 
     ylim=c(min(y.filt),1.1*max(y.filt)), main="Detected Positions",
     xlab="X", ylab="Y")
alphaPal <- colorRampPalette(c('red', 'white', 'blue'))
a.col<-alldata$alpha
a.col[which(a.col>30)]<-30
a.col[which(a.col<(-30))]<--30
alphaCol <- alphaPal(10)[as.numeric(cut(a.col, breaks = 10))]
# points(alldata$x.filt[-1], alldata$y.filt[-1], cex=0.5, pch = 21, col=alphaCol,
#        bg=alphaCol)
points(alldata$X.Pos[-1], alldata$Y.Pos[-1], cex=0.5, pch = 21, col=alphaCol,
       bg=alphaCol)
legend(x="topright", c("Right turn", "Left turn"), col=c("red", "blue"), pch=c(1,1), 
       bty="n")




#9.  Plot the trajectory data ####                                
par(mfrow=c(1,2))
polar.plot(velocity, theta, line.col="red", point.col="red", rp.type="s", 
           start=0, main=("Movement (° vs. cm/s) of Fish"), cex=0.2)
title(sub="Angle (°) is relative to horizontal = 0°", line=1)
polar.plot(acceleration[-1], alpha[-1], point.col="red", start=90, rp.type="s",
           main="Turning Angle (° vs. cm/s/s)", cex=0.2,
           labels=c(0,20,40,60,80,100,120,140,160,180,-160,-140,-120,-100,
                    -80,-60,-40,-20))
title(sub="+° = left, -° = right", line=1)

bin.labels<-seq(-172.5,172.5,15)
bins<-seq(-180,180,15)

par(mfrow=c(1,1))
freq.alpha<-as.numeric(table(cut(alpha, breaks=bins, labels=bin.labels)))
polar.plot(100*freq.alpha/sum(freq.alpha), bin.labels, line.col="black", 
           rp.type="r", start=90, lwd=10, lend=1,
           main="Distribution of Alphas (% of observed turning angles)",
           labels=c(0,20,40,60,80,100,120,140,160,180,-160,-140,-120,-100,
                    -80,-60,-40,-20),
           mar=c(5,5,5,5))
title(sub="+° = left, -° = right", line=1)

par(oldpar)
par(mfrow=c(2,2), mar=c(5,5,2,5))
hist(velocity, main="Velocity", xlab="Velocity (cm/s)")
hist(theta, main="Thetas", xlab="Angle (°)", ylab="# of observations")
hist(acceleration[-1], main="Acceleration", xlab="Acceleration (cm/s/s)", 
     ylab="# of observations")
hist(alpha[-1], main="Alphas", xlab="Angle (°)", ylab="# of observations" )

mean(alpha, na.rm=TRUE)
par(mfrow=c(1,2), mar=c(5,5,5,5))
hist(na.omit(alpha), main="Frequency of all alphas", xlab="Turning angle (°)", 
     ylab="# of observations")
hist(na.omit(alpha[which(abs(alpha)>20)]), main="Frequency of | alphas | > 20°",
     xlab="Turning angle (°)", ylab="# of observations")

par(mfrow=c(1,1))
freq.alpha20<-as.numeric(table(cut(alpha[which(abs(alpha)>20)], breaks=bins,
                                   labels=bin.labels)))
polar.plot(100*freq.alpha20/sum(freq.alpha20), bin.labels, line.col="black", 
           rp.type="r", start=90, lwd=10, lend=1,
           main="Distribution of | alphas | > 20° (% of observed turning angles)",
           labels=c(0,20,40,60,80,100,120,140,160,180,-160,-140,-120,-100,
                    -80,-60,-40,-20),
           mar=c(5,5,5,5))
title(sub="+° = left, -° = right", line=1)


# Cumulative Alpha calculation ####
# calculative cumulative alpha and reset every time the fish switches sides (event)
alldata$cumalpha<-alphacumulate(alldata$alpha, alldata$Event)/alldata$Delta.time


#10. Subset data by side ####
pdr<-subset(alldata, Fish.Side=="RIGHT", row.names=NULL)
pdl<-subset(alldata, Fish.Side=="LEFT", row.names=NULL)

par(oldpar)

par(mfrow=c(3,1), mar=c(5,5,1,2))
plot(overallslope, type="l")
plot(alldata$x.filt, type="l")
plot(fs, type="l")

par(oldpar)
par(mfrow=c(1,1), mar=c(5,5,5,3))
plot(alldata$Time.diff, alldata$fish.temp, type="l", col="black", main="Behavioural thermoregulation",
     xlab="Elapsed time (s)", ylab="Selected temperature (°C)")
points(pdr$Time.diff, pdr$fish.temp, col="red", cex=0.2)
points(pdl$Time.diff, pdl$fish.temp, col="blue", cex=0.2)
points(alldata$Time.diff[le.index], let, col="blue")
points(alldata$Time.diff[ue.index], uet, col="red")


par(mfrow=c(1,2), mar=c(5,5,5,5))
hist(let, main="Lower Escape Temperatures", col="blue")
hist(uet, main="Upper Escape Temperatures", col="red")
stat.desc(let)
stat.desc(uet)
mean(fish.temp)
mean(let)
mean(uet)

par(mfcol=c(2,2), mar=c(5,5,2,2))
hist(alldata$velocity[le.index], col="blue")
hist(alldata$velocity[ue.index], col="red")

hist(alldata$acceleration[le.index], col="blue")
hist(alldata$acceleration[ue.index], col="red")

par(mfrow=c(1,1), mar=c(5,5,2,5))
smoothScatter(alldata$fish.temp, alldata$velocity, nbin=256,
              colramp=heat.colors,
              main="Swimming Velocity ~ Selected Temperature",
              xlab="Selected Temperature",
              "ylab"="Velocity",
              postPlotHook=addgradient)
fit<-lm(velocity~fish.temp, data=alldata)
abline(coef=fit$coef, xpd=FALSE)


#11. Plot trajectory data based on side ####
par(mfrow=c(1,1), mar=c(5,5,2,2))
plot(pdr$Time.diff, pdr$velocity, type="p", col="red", main="Velocity by side",
     xlab="Elapsed time (s)", ylab="Velocity (cm/s)", cex=0.2)
points(pdl$Time.diff, pdl$velocity, col="blue", cex=0.2)

plot(pdr$Time.diff, pdr$acceleration, type="p", col="red", main="Acceleration by side",
     xlab="Elapsed time (s)", ylab="Acceleration (cm/s/s)", cex=0.2)
points(pdl$Time.diff, pdl$acceleration, col="blue", cex=0.2)

plot(pdr$Time.diff, pdr$alpha, type="p", col="red", main="Alpha by side",
     xlab="Elapsed time (s)", ylab="Turning angle", cex=0.2)
points(pdl$Time.diff, pdl$alpha, col="blue", cex=0.2)

par(mfrow=c(1,1), xpd=TRUE)
polar.plot(pdr$velocity, pdr$theta, point.col="red", rp.type="s", start=0, 
           main="Movement vectors (° vs. cm/s)", cex=0.2, mar=c(4,4,4,4))
polar.plot(pdl$velocity, pdl$theta, point.col="blue", cex=0.2, rp.type="s", start=0, 
           add=TRUE)
legend(x="topright", c("Right side", "Left side"), col=c("red", "blue"), pch=c(1,1), 
       bty="n", inset=c(-0.2,0))
par(mfrow=c(1,1), xpd=TRUE)
polar.plot(pdr$acceleration[-1], pdr$alpha[-1], point.col="red", start=90, rp.type="s", 
           main="Turning vectors (° vs. cm/s/s)", mar=c(4,4,4,4), cex=0.2)
polar.plot(pdl$acceleration[-1], pdl$alpha[-1], point.col="blue", start=90, rp.type="s",
           add=TRUE, cex=0.2)
legend(x="topright", c("Right side", "Left side"), col=c("red", "blue"), pch=c(1,1), 
       bty="n", inset=c(-0.2,0))
par(oldpar)


plot(cumsum(na.omit(alldata$alpha)), main="Cumulative Alpha")
plot(cumsum(na.omit(pdl$alpha)), main="Cumulative Alpha Left")
plot(cumsum(na.omit(pdr$alpha)), main="Cumulative Alpha Right")

plot(cumsum(na.omit(alldata$distance)), main="Cumulative Distance")
plot(cumsum(na.omit(pdl$distance)), main="Cumulative Distance Left")
plot(cumsum(na.omit(pdr$distance)), main="Cumulative Distance Right")

# if you take the cumulative sum of the instantaneous turning angles, you can derive an overall
# impression of whether one direction dominates more than the other.
# also, on each side of the shuttle box, the cumulative sum will indicate if turns occur more
# in one way or the other


library(ggplot2)
g<-ggplot(data=pdl, aes(x=1:nrow(pdl), y=cumalpha))+
  geom_point(aes(col=alpha), alpha=1)+
  scale_color_gradientn(colors=ironbowpal, limits=c(-50,50))
g

g<-ggplot(data=pdr, aes(x=1:nrow(pdr), y=cumalpha))+
  geom_point(aes(col=alpha), alpha=1)+
  scale_color_gradientn(colors=ironbowpal, limits=c(-50,50))
g



g<-ggplot(data=alldata, aes(x=x.filt, y=y.filt))+
  geom_point(aes(col=alpha, size=abs(alpha)), alpha=1)+
  scale_color_gradientn(colors=ironbowpal, limits=c(-50,50))
g

g<-ggplot(data=pdl, aes(x=x.filt, y=y.filt))+
  geom_point(aes(col=alpha), size=1, alpha=0.5)+
  scale_color_gradientn(colors=ironbowpal, limits=c(-20,20))
g

g<-ggplot(data=pdr, aes(x=x.filt, y=y.filt))+
  geom_point(aes(col=alpha), size=1, alpha=0.5)+
  scale_color_gradientn(colors=ironbowpal, limits=c(-20,20))
g

# Area Plot #### 
# size of fish ~ Area
g<-ggplot(data=alldata, aes(x=x.filt, y=y.filt))+
  ggtheme()+
  geom_point(aes(col=Area, size=Area),  alpha=0.2)+
  scale_size_continuous(range=c(0.5,3))+
  scale_color_gradientn(colors=ironbowpal)+
  xlab("X (cm)")+ylab("Y (cm)")
g




#12. Summarise and Output #####
options(scipen=999)
colstats<-c("fish.temp", "Delta.time", "velocity", "theta", "acceleration", "alpha")
overall.sum<-stat.desc(alldata[colstats], basic=TRUE, desc=TRUE, norm=FALSE, p=0.95)
overall.sum
right.sum<-stat.desc(pdr[colstats], basic=TRUE, desc=TRUE, norm=FALSE, p=0.95)
right.sum
left.sum<-stat.desc(pdl[colstats], basic=TRUE, desc=TRUE, norm=FALSE, p=0.95)
left.sum

alldata$TimeIndex<-floor(alldata$Time.diff/60)
tempsum<-ddply(alldata, .(TimeIndex), summarize, MeanTemp=mean(fish.temp, na.rm=TRUE))
velocitysum<-ddply(alldata, .(TimeIndex), summarize, MeanVelocity=mean(velocity, na.rm=TRUE))
thetasum<-ddply(alldata, .(TimeIndex), summarize, MeanTheta=mean(theta, na.rm=TRUE))
accelerationsum<-ddply(alldata, .(TimeIndex), summarize, MeanAcceleration=mean(acceleration, na.rm=TRUE))
alphasum<-ddply(alldata, .(TimeIndex), summarize, MeanAlpha=mean(alpha, na.rm=TRUE))
areasum<-ddply(alldata, .(TimeIndex), summarize, MeanArea=mean(Area, na.rm=TRUE))

# merge 3 data frames at once:
temp<-list(tempsum, velocitysum, thetasum, accelerationsum, alphasum, areasum)
fishtimesum<-Reduce(function(x, y) merge(x, y, all=TRUE, sort=F), temp)


# Finalise summary data
 Expt.Duration<-NA
 Time.low<-NA
 Time.upp<-NA
 Duration.low<-NA
 Duration.upp<-NA
 No.shuttles<-NA
 LET.min<-NA
 LET.max<-NA
 LET.avg<-NA
 LET.med<-NA
 LET.sd<-NA
 UET.min<-NA
 UET.max<-NA
 UET.avg<-NA
 UET.med<-NA
 UET.sd<-NA
 Area.med<-NA
 TFish.avg<-NA
 Tfish.med<-NA
 Tfish.sd<-NA
 Vel.avg<-NA
 Vel.med<-NA
 Theta.avg<-NA
 Theta.med <-NA
  Accel.avg<-NA
  Accel.med<-NA
  Alpha.avg <-NA
  Alpha.med<-NA
  Vel.avg.f<-NA
  Accel.avg.f<-NA
  Theta.avg.f <-NA
  Alpha.avg.f <-NA
  CumulDist.Left <-NA
  CumulDist.Right<-NA
  Bias.index<-NA
  Bias.strength<-NA
 
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
LET.sd<-sd(leshuttles$LET, na.rm=TRUE)

UET.min<-min(ueshuttles$UET, na.rm=TRUE)
# minimum upper escape temperature
UET.max<-max(ueshuttles$UET, na.rm=TRUE)
# maximum upper escape temperature
UET.avg<-mean(ueshuttles$UET, na.rm=TRUE)
# average upper escape temperature
UET.med<-median(ueshuttles$UET, na.rm=TRUE)
# median upper escape temperature
UET.sd<-sd(ueshuttles$UET, na.rm=TRUE)

Area.med<-median(alldata$Area, na.rm=TRUE)

#### No filtering
TFish.avg<-mean(alldata$fish.temp, na.rm=TRUE)
# overall average temperature of the fish, based on total time spent at any given temperature
Tfish.med<-median(alldata$fish.temp, na.rm=TRUE)
# overall median temperature of the fish, based on total time spent at any given temperature
Tfish.sd<-sd(alldata$fish.temp, na.rm=TRUE)

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

CumulDist.Left<-sum(pdl$distance)
# cumulative distance moved when on the left side
CumulDist.Right<-sum(pdr$distance)
# cumulative distnace moved when on the right side

filtered.data<-subset(alldata, velocity>Vel.med & abs(alpha)>20)
# create new data according to filter of velocities>median and alphas>20 degrees

counterclockwise<-subset(filtered.data[colstats], alpha>20)
# subset only those alphas that are >20 degrees into counterclockwise dataframe
clockwise<-subset(filtered.data[colstats], alpha<(-20))
# subset only those alphas that are < -20 degrees into clockwise dataframe

n.counterclockwise<-nrow(counterclockwise)
n.clockwise<-nrow(clockwise)

Bias.index<-(n.clockwise-n.counterclockwise)/(n.clockwise+n.counterclockwise)
# bias.index is the ratio of the difference in turns to the total counted turns
# -1 = all left, counterclockwise turns
# +1 = all right, clockwise turns
# 0 = no difference in turns
Bias.strength<-abs(mean(clockwise$alpha, na.rm=T)+mean(counterclockwise$alpha, na.rm=T))

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
                   LET.min, LET.max, LET.avg, LET.med, LET.sd,
                   UET.min, UET.max, UET.avg, UET.med, UET.sd,
                   Area.med,
                   TFish.avg, Tfish.med, Tfish.sd, Vel.avg, Vel.med, Theta.avg, Theta.med, 
                   Accel.avg, Accel.med, Alpha.avg, Alpha.med,
                   Vel.avg.f, Accel.avg.f, Theta.avg.f, Alpha.avg.f, 
                   CumulDist.Left, CumulDist.Right,
                   Bias.index, Bias.strength)

t(summaryoutput)

setwd(outputDir)

if(file.exists(fileout))
{
  write.table(summaryoutput, fileout, append=TRUE, sep=",", col.names=FALSE, row.names=FALSE)
}
if(!file.exists(fileout))
{
  write.table(summaryoutput, fileout, append=TRUE, sep=",", col.names=TRUE, row.names=FALSE) 
}

# Final File prep with processed data
processed<-list(overall.sum, summaryoutput, pdl, pdr, filtered.data, clockwise,
                counterclockwise, leshuttles, ueshuttles, allshuttles, fishtimesum)
names(processed)<-c("overall.sum", "summaryoutput", "pdl", "pdr", "filtered.data", "clockwise",
                    "counterclockwise", "leshuttles", "ueshuttles", "allshuttles", "fishtimesum")
saveRDS(processed, file=Rdatafileout)
p<-readRDS(Rdatafileout)

write.csv(fishtimesum, file=timesummaryout, row.names=F )
write.csv(allshuttles, file=shuttlesummaryout, row.names=F)


#} # this closes the m loop used for randomisation procedure
#} # this closes the f in l.files[] loop