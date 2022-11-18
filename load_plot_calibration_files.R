# Load all calibration logs and compare RV values for constant standard, all sondes

rm(list=ls())

{
indir.5259.ibwc = "K:/Shared drives/Project - Tijuana River Monitoring/data/sonde_IBWC_12215259/calibration_logs/"
indir.3594.MT = "K:/Shared drives/Project - Tijuana River Monitoring/data/sonde_MT_MT02193594/calibration_logs/"
indir.4105 = "K:/Shared drives/Project - Tijuana River Monitoring/data/sonde_TJE_01204105L_/calibration_logs/"
}

# Get file details so you get the most recent calibration log
{
flist.5259 = list.files(indir.5259.ibwc,pattern="*.csv")
file.details.5259 = file.info(list.files(indir.5259.ibwc,pattern="*.csv",full.names=TRUE))
file.ctime.5259 = as.POSIXlt(file.details.5259[,c("ctime")])
fname.most.recent.5259 = flist.5259[which(file.ctime.5259==max(file.ctime.5259))]

flist.3594 = list.files(indir.3594.MT,pattern="*.csv")
file.details.3594 = file.info(list.files(indir.3594.MT,pattern="*.csv",full.names=TRUE))
file.ctime.3594 = as.POSIXlt(file.details.3594[,c("ctime")])
fname.most.recent.3594 = flist.3594[which(file.ctime.3594==max(file.ctime.3594))]

flist.4105 = list.files(indir.4105,pattern="*.csv")
file.details.4105 = file.info(list.files(indir.4105,pattern="*.csv",full.names=TRUE))
file.ctime.4105 = as.POSIXlt(file.details.4105[,c("ctime")])
fname.most.recent.4105 = flist.4105[which(file.ctime.4105==max(file.ctime.4105))]
}

# Load the calibration logs
{
  x.5259 = read.csv(paste0(indir.5259.ibwc,fname.most.recent.5259))
  x.3594 = read.csv(paste0(indir.3594.MT,fname.most.recent.3594),skip=1)
  x.4105 = read.csv(paste0(indir.4105,fname.most.recent.4105),skip=1)
}

# Date time in posix
{
  x.5259$Date.time = as.POSIXlt(x.5259$date)
  x.5259$Date = as.Date(x.5259$Date.time)
  x.3594$Date = as.Date(x.3594$Date,format=c("%m/%d/%y"))
  x.4105$Date = as.Date(x.4105$Date,format=c("%m/%d/%y"))
}

sonde.list = c(5259,3594,4105)
sonde.plot.col = c("black","blue","green")
# Fix names (some are upper case, some lower, different names for SN)

for (s in 1:length(sonde.list)){
    sonde = sonde.list[s]
    eval(parse(text=paste0("x=x.",sonde)))  # Assign the variable x.sondenum to "x"
    x.names = names(x)
    index.var = which(x.names=="sensor"|(x.names=="Sensor"))
    names(x)[index.var] = "sensor"  # Standard name - "sensor"
    
    index.var = which(x.names=="rv"|(x.names=="RV"))
    names(x)[index.var] = "rv"  # Standard name - "rv"
    
    index.var = which(x.names=="New"|(x.names=="new"))
    names(x)[index.var] = "new"  # Standard name - "new"
    
    index.var = which(x.names=="Old"|(x.names=="old"))
    names(x)[index.var] = "old"  # Standard name - "old"
    
    index.var = which(x.names=="X"|(x.names=="comment"))
    names(x)[index.var] = "done"  # Standard name - "done"
    eval(parse(text=paste0("x.",sonde,"=x")))  # Assign x to x.sonde
}

# For TRYP, CDOM and TURB, extract cal log data for each param, plot RV value for each

combine.callogs <- function(parm){
  for (s in 1:length(sonde.list)){
    sonde = sonde.list[s]
    #parm = "TRYP"
    eval(parse(text=paste0("x=x.",sonde)))  # Assign the variable x.sondenum to "x"
    x.par = x[x$sensor==parm,c("Date","sensor","rv","old","new","done")]
    x.par$sonde = sonde
    x.par$plotcol = sonde.plot.col[s]
    # Scatterplot of rv vs new
    plot(x.par$new,x.par$rv,xlab="New ppb",ylab="RV")
    if (s==1){
      x.out = x.par
    } else {
      x.out = as.data.frame(rbind(x.out,x.par))
    }
  }
  return(x.out)
}

xcomb.TRYP = combine.callogs(parm="TRYP")
xcomb.CDOM = combine.callogs(parm="CDOM")
# Go through each date, select last measured TYRP value for 0 and 100


# Overall scatterplot
plot(xcomb.TRYP$new,xcomb.TRYP$rv,col=xcomb.TRYP$plotcol,pch=20,ylab="TRYP RV",xlab="New ppb")

# Plot time series for each sonde
xlims.date = range(xcomb.TRYP$Date)
ylims.rv = range(xcomb.TRYP$rv)
xcomb.TRYP.100 = xcomb.TRYP[xcomb.TRYP$new==100,]
plot(xcomb.TRYP.100$Date,xcomb.TRYP.100$rv,col=xcomb.TRYP.100$plotcol,pch=20,ylim=ylims.rv,cex=2,ylab="TRYP RV",main="Time series of RV value for TRYP=100ppb standard")
legend("topleft",legend=sonde.list,col=sonde.plot.col,pch=20,pt.cex=1.5)


mean.rv.tryp.100.sonde.3594.pre2021 = mean(xcomb.TRYP.100[(xcomb.TRYP.100$Date<=as.Date("2021-01-01"))&(xcomb.TRYP.100$sonde==3594),"rv"])

mean.rv.tryp.100.sonde.3594.post2021.low.values = mean(xcomb.TRYP.100[(xcomb.TRYP.100$Date>=as.Date("2021-01-01"))&(xcomb.TRYP.100$sonde==3594) & (xcomb.TRYP.100$rv<0.03),"rv"])


xlims.date = range(xcomb.TRYP$Date)
ylims.rv = range(xcomb.TRYP$rv)
xcomb.TRYP.100 = xcomb.TRYP[xcomb.TRYP$new==100,]
plot(xcomb.TRYP.100$Date,xcomb.TRYP.100$rv,col=xcomb.TRYP.100$plotcol,pch=20,ylim=ylims.rv,xlab="",cex=2,ylab="TRYP RV",main="Time series of RV value for TRYP=100ppb standard")
legend("topleft",legend=sonde.list,col=sonde.plot.col,pch=20,pt.cex=1.5)

# Calculate mean RV for different periods
mean.rv.tryp.100.sonde.3594.pre2021 = xcomb.TRYP.100[(xcomb.TRYP.100$Date<=as.Date("2021-01-01"))&(xcomb.TRYP.100$sonde==3594),]

mean.rv.tryp.100.sonde.3594.pre2021 = mean(xcomb.TRYP.100[(xcomb.TRYP.100$Date<=as.Date("2021-01-01"))&(xcomb.TRYP.100$sonde==3594),"rv"])

mean.rv.tryp.100.sonde.3594.post2021.low.values = mean(xcomb.TRYP.100[(xcomb.TRYP.100$Date>=as.Date("2021-01-01"))&(xcomb.TRYP.100$sonde==3594) & (xcomb.TRYP.100$rv<0.03),"rv"])

lines(x=as.Date("2019-01-01","2021-01-01"),rep(mean.rv.tryp.100.sonde.3594.pre2021,2),col="blue",lty=2)
lines(x=as.Date("2021-01-01","2022-12-01"),rep(mean.rv.tryp.100.sonde.3594.post2021.low.values,2),col="blue",lty=2)

