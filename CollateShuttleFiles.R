# Set to working directory where the shuttle summary files are contained

setwd("~/Desktop/Justin/21C 2016 Ramping Outputs") # YOU CHANGE THIS!
outputfilename<-"21C_2016_Ramping.csv" # YOU CHANGE THIS!!

l.files <- dir(pattern="by_shuttle") # where you have your files
j.files<-dir()
l.files

readcsv_addID<-function(f.name){
  rootname<-gsub(pattern="_by_shuttle.csv", replacement="", f.name)
  d<-read.csv(f.name)
  # res<-d[1,]
  res<-data.frame(ID=rootname, d)
  return(res)
}

readcsv_firstrow<-function(f.name){
  d<-read.csv(f.name)
  res<-data.frame(ID=f.name, d[1,])
  return(res)
}

# Double check first that all files contain same # of columns:
for(f in l.files){
  d<-read.csv(f)
  dims<-ncol(d)
  print(paste(f, "contains", dims, "columns"))
}

# If they all contain the same number of columns, then you can run the following:

dsmerge <- do.call(rbind,lapply(l.files, readcsv_addID))
#dsmerge <- do.call(rbind,lapply(l.files, readcsv_firstrow)) # CHANGE THIS FOR EXTRACTING 1st ROW of Phil's FILES
str(dsmerge)

setwd("~/Desktop/Justin/CollateOutput")
write.csv(dsmerge, file=outputfilename)
