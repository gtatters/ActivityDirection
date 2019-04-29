# Produce a spiral plot
library(plotrix)

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
  magnitude<-sqrt(dy^2+dx^2)
  # remove really small changes (i.e. zmin=3 pixels or less)
  dx[which(abs(magnitude)<zmin)]<-NA
  dy[which(abs(magnitude)<zmin)]<-NA
  
  theta<-180*atan2(dy,dx)/pi
  #theta<-180*atan(dy/dx)/pi
  #theta[which(dx<0 & dy>0)]<-theta[which(dx<0 & dy>0)]+180
  #theta[which(dx<0 & dy<0)]<-theta[which(dx<0 & dy<0)]+180
  #theta[which(dx>0 & dy<0)]<-theta[which(dx>0 & dy<0)]+360
  
  alpha<-diff(theta)
  alpha[which(alpha>180)]<-(-360+alpha[which(alpha>180)])
  alpha[which(alpha<(-180))]<-360+alpha[which(alpha<(-180))]
  change<-diff(magnitude)
  
  #change[which(abs(change)<zmin)]<-NA
  #alpha[which(abs(change)<zmin)]<-NA
  
  change<-c(NA,change)
  alpha<-c(NA,alpha)
  
  zz<-data.frame(magnitude,theta,change,alpha)
}

#right turning spiral
a<-5
t<-seq(0,10,0.1)
x.right<-a*t*sin(t)
x.right<-jitter(x.right,100,4) # add noise to the data
y.right<-a*t*cos(t)
y.right<-jitter(y.right,100,4) # add noise to the data

x.right.filt<-sgolayfilt(x.right, 9, 13) 
y.right.filt<-sgolayfilt(y.right, 9, 13)
plot(x.right)
lines(x.right.filt, col="blue")
plot(x.right.filt, y.right.filt)

par(mfrow=c(1,1))
plot(x.right, y.right, type="p", main="Right Turning Spiral")

bearing.right<-bearing(x.right.filt,y.right.filt,0)
mag.right<-bearing.right[,1]
theta.right<-bearing.right[,2]
change.right<-bearing.right[,3]
alpha.right<-bearing.right[,4]

polar.plot(bearing.right[,1], bearing.right[,2], line.col="red", point.col="red", rp.type="s", 
           start=0, main=("Movement Bearing (°) of Right Turn Spiral"))
polar.plot(change.right[-1], alpha.right[-1], point.col="red", start=90, rp.type="s",
           main="Change in Direction (°), +ve is left, -ve is right",
           labels=c(0,20,40,60,80,100,120,140,160,180,-160,-140,-120,-100,-80,-60,-40,-20))

par(mfrow=c(2,2))
hist(bearing.right[,1], main="Magnitudes")
hist(bearing.right[,2], main="Thetas")
hist(change.right[-1], main="Direction changes")
hist(alpha.right[-1], main="Alphas")

#left turning spiral
a<-5
t<-seq(0,10,0.1)
x.left<-a*t*cos(t)
y.left<-a*t*sin(t)

par(mfrow=c(1,1))
plot(x.left,y.left, type="p", main="Left Turning Spiral")

bearing.left<-bearing(x.left,y.left,0)
alpha.left<-bearing.left[,4]
change.left<-bearing.left[,3]

polar.plot(bearing.left[,1], bearing.left[,2], line.col="red", point.col="red", rp.type="s", 
           start=0, main=("Movement Bearing (°) of Left Turning Spiral"))
polar.plot(change.left[-1], alpha.left[-1], point.col="red", start=90, rp.type="s",
           main="Change in Direction (°), +ve is left, -ve is right",
           labels=c(0,20,40,60,80,100,120,140,160,180,-160,-140,-120,-100,-80,-60,-40,-20))

par(mfrow=c(2,2))
hist(bearing.left[,1], main="Magnitudes")
hist(bearing.left[,2], main="Thetas")
hist(change.left[-1], main="Direction changes")
hist(alpha.left[-1], main="Alphas")

# Random movement
dx.rand<-sample(-10:10,999,replace=TRUE)
dy.rand<-sample(-10:10,999,replace=TRUE)

x.rand<-cumsum(c(0,dx.rand))
y.rand<-cumsum(c(0,dy.rand))

par(mfrow=c(1,1))
plot(x.rand,y.rand,type="l", main="Random Walk")

bearing.rand<-bearing(x.rand,y.rand,-1)
alpha.rand<-bearing.rand[,4]
change.rand<-bearing.rand[,3]

polar.plot(bearing.rand[,1], bearing.rand[,2], line.col="red", point.col="red", rp.type="s", 
           start=0, main=("Movement Bearing (°)"))
polar.plot(change.rand[-1], alpha.rand[-1], point.col="red", start=90, rp.type="s",
           main="Change in Direction (°), +ve is left, -ve is right",
           labels=c(0,20,40,60,80,100,120,140,160,180,-160,-140,-120,-100,-80,-60,-40,-20))

par(mfrow=c(2,2))
hist(bearing.rand[,1], main="Histogram of magnitudes")
hist(bearing.rand[,2], main="Histogram of thetas")
hist(change.rand[-1], main="Histogram of direction changes")
hist(alpha.rand[-1], main="Histogram of alphas")
