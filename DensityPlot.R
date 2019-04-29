
library(Thermimage)
library(fields)
library(xlsx)

mainDir<-"~/Dropbox/R/MyProjects/ActivityDirection/"
dataDir<-paste0(mainDir, "data/")
setwd(dataDir)

alldata<-read.csv("xydata.csv")
alldata<-read.xlsx("February8.xlsx", sheetIndex=7) # compare Sheet 2 vs 3 
colnames(alldata)

# # Plot Fish smoothed (X,Y) data ####
# par(mfrow=c(1,1), mar=c(5,5,5,5))
# plot(alldata$X, alldata$Y, type="l", 
#      main="Fish Positions - Raw", xlab="x", ylab="y")

# #smooth.palette <- colorRampPalette(blues9[-(1:2)])
# smooth.palette <- palette.choose("glowbow")
# smoothScatter(alldata$X, alldata$Y, nbin=256, bty="n",
#               xlab="X Axis - Pixels", ylab = "Y Axis - Pixels",
#               #xlim=c(0,1100),# ylim=c(0,240),
#               main="Density Plot of Fish Positions - Raw",
#               colramp=colorRampPalette(smooth.palette),
#               xlim=c(min(alldata$X),max(alldata$X)),
#               ylim=c(min(alldata$Y),max(alldata$Y)),
#               postPlotHook=addgradient)


# Plot temperature image using ggplot2
library(ggplot2)
p<-ggplot(data = alldata, aes(X, Y)) +
  stat_density2d(aes(fill = ..density..^0.25), geom = "raster", contour = FALSE, n = 200) +
  scale_fill_gradientn(colours = glowbowpal)+
  theme(
    axis.line = element_blank(), 
    axis.ticks = element_blank(), 
    axis.text = element_blank(), 
    text = element_blank(), 
    plot.margin = unit(c(0,0,0,0), "cm")
  )
print(p)

ggsave(
   #"../images/plotNo1.jpg", 
   "~/Desktop/plotno7.pdf",
   plot = last_plot(), 
   width = 54.19, 
   height = 13.55, 
   dpi = 300, 
   units = "cm"
 )

