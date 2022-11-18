#  Load raw wqdatalive data
#  Load calibration log
#  Back-calibrate raw RV values for TRYP, CDOM, turbidity

rm(list=ls())  # clear variables

{
indir.wqdl = "K:/Shared drives/Project - Tijuana River Monitoring/data/sonde_IBWC_12215259/wqdatalive_downloads/1_raw/2022/"
flist = list.files(indir.wqdl)
outdir.wqdl.w.alldata = "K:/Shared drives/Project - Tijuana River Monitoring/data/sonde_IBWC_12215259/wqdatalive_downloads/2_raw_with_RV_alldata/"
outdir.wqdl = "K:/Shared drives/Project - Tijuana River Monitoring/data/sonde_IBWC_12215259/wqdatalive_downloads/3_post_QAQC/"

# Load the last wqdatalive file in the list (most recent)
setwd(indir.wqdl)
fname = flist[length(flist)]  # **** Can change which file to read in here.
x = read.csv(fname,skip=2)
}

# Find and replace all "--" with NA and convert to numeric
{
x[x=="--"] = NA
xdata = x[,2:ncol(x)]
xdata = as.data.frame(sapply(xdata,as.numeric))
xdf = data.frame(Date.time=strptime(x$Parameter...Timestamp,format="%m-%d-%Y %H:%M:%S"),xdata)
}

# Rename wqdatalive parameters with better text (raw data text is full of extra .., etc)
{
pretty.parmname.list = as.data.frame(rbind(c("Date.time","Date.time"),c("Battery..V.","Battery.V"),c("Temperature..C.","Temp.C"),c("pH","pH"),c("ORP..mV.","ORP.mV"),c("Depth..m.","Depth.m"),c("Sp.Cond..uS.cm.","Cond.uS.cm"),c("Turbidity.FNU..FNU.","Turb.Manta.FNU"),c("DOSat....","DO.pct.sat"),c("DO..mg.L.","DO.mgL"),c("CDOM..ug.L.","CDOM.ugL"),c("Tryptophan..ppb.","TRYP.ppb"),c("pH.mV","pH.mV"),c("Internal.Bat..V.","Battery.OBS.V"), c("Backscatter.Turbidity..TU.","Turb.BS.TU"),c("Sidescatter.Turbidity..TU.","Turb.SS.TU"),c("Temperature..C..1","Temp.OBS.C"),c("Wet.Dry.Flag","Wet.Dry.Flag")))
names(pretty.parmname.list) = c("wqdl","pretty")
xdf.names.df = data.frame(wqdl=names(xdf),foo=1)
pretty.names.wqdl = merge(xdf.names.df,pretty.parmname.list,all.x=TRUE,by="wqdl",sort=FALSE)
  #  Check to make sure that no NA values remain...meaning that pretty.parmname.list has all variables present in xdf
check.names.match = pretty.names.wqdl$pretty[is.na(pretty.names.wqdl$pretty)]  # should be character(0)
if (length(check.names.match)>0){
  print("Names mismatch in creating pretty names for xdf")
}
names(xdf) = pretty.names.wqdl$pretty
}

#  Load calibration log for this sonde
{
indir.callog = "K:/Shared drives/Project - Tijuana River Monitoring/data/sonde_IBWC_12215259/calibration_logs/"
flist = list.files(indir.callog,pattern="csv")
setwd(indir.callog)
xcallog = read.csv(flist)
xcallog$Date.time = as.POSIXlt(xcallog$date)
xcallog$Date = as.Date(xcallog$Date.time)
most.recent.cal.date = max(xcallog$Date)  # Here, can change to whatever calibration date you'd like to back-calibrate with.
}

parm.names.for.backcal.RV = data.frame(cal.names = c("TRYP","CDOM","TURB"),c("TRYP.ppb","CDOM.ugL","Turb.Manta.FNU"))

# Make back-calibration of RV into a function
backcal.RV <- function(parm.callog,parm.xdf){  # 2 arguments:  parm name in the Calibration logs, and parm name in the wqdatalive dataframe (pretty name)
  xcal.parm = xcallog[(xcallog$Date==most.recent.cal.date) & (xcallog$sensor==parm.callog),c("sensor","rv","new")]  # RV is observed RV, new is the "true" calibration value (0 or calibration ppb)
  if (nrow(xcal.parm)>2){
    print("More than 2 calibration points for TRYP")   # Can decide what to do from there--see if differences are large for a single point, leave all points in, etc.
  }
  if ((nrow(xcal.parm)==1) & (max(xcal.parm$new)>0)){
    print("No zero point in the calibration.  Assuming RV value at new calibration value is zero")
    xcal.parm = as.data.frame(rbind(xcal.parm,c(parm.callog,0,0)))
    xcal.parm$rv = as.numeric(xcal.parm$rv)  # rbind turns data back into character.
    xcal.parm$new = as.numeric(xcal.parm$new)
  }
  lm.parm = lm(rv~new,data=xcal.parm)
  xdf.tmp = data.frame(new=xdf[,parm.xdf],foo=1)  # Add foo=1 column; predict doesn't like a data.frame with one column.
  
  rv.predict = predict.lm(lm.parm,new=xdf.tmp)
  
  # Plot time series of original value and RV to make sure they look right.
  {
    par(mar=c(1,5,1,3))
    plot(xdf$Date.time,xdf[,parm.xdf],type="l",lwd=3,col="black",las=1,ylab="") 
    mtext(side=2,parm.xdf,line=3.5)
    par(new=TRUE)  # Secondary axis
    plot(xdf$Date.time,rv.predict,col="red",xlab="",yaxt="n",ylab="",las=1,cex=0.7)
    axis(side=4)
    mtext(side=4,"RV",line=2.5)
  }
  return(rv.predict)
}

# Load in quality flags and notes
{
indir.qaqc.notes = "K:/Shared drives/Project - Tijuana River Monitoring/data/sonde_IBWC_12215259/wqdatalive_downloads/"
flist.qaqc = list.files(indir.qaqc.notes,pattern="csv")
qaqc.length = length(flist.qaqc)
setwd(indir.qaqc.notes)
x.qaqc = read.csv(flist.qaqc)
x.qaqc$Start.date.time = strptime(x.qaqc$Start,format="%m/%d/%Y %H:%M")
x.qaqc$End.date.time = strptime(x.qaqc$End,format="%m/%d/%Y %H:%M")
}

# Add rectangle to plot showing bad QAQC for all parameters
#xdf$TRYP.RV = backcal.RV(parm.callog="TRYP",parm.xdf="TRYP.ppb")

plot.rect.nodata <- function(parname.xdf){  # Plot grey rectangles where there is no-data for all parameters
  x.qaqc.ALL = x.qaqc[x.qaqc$Parameter=="ALL",]  # Only where all parameters were bad
  for (r in 1:nrow(x.qaqc.ALL)){
  x.qaqc.sub = x.qaqc.ALL[r,]
  rect(xleft=x.qaqc.sub$Start.date.time,ybottom=0,xright=x.qaqc.sub$End.date.time,ytop=max(xdf[,parname.xdf],na.rm=TRUE),col= rgb(0.5,0.5,0.5,alpha=0.5))
  text(x=x.qaqc.sub$Start.date.time,y=0.9*max(xdf[,parname.xdf],na.rm=TRUE),labels=x.qaqc.sub$Comment, cex=0.8)
}
}

# Plot parameters where doing back-calibration of RV
{
par(mfrow=c(3,1))
xdf$TRYP.RV = backcal.RV(parm.callog="TRYP",parm.xdf="TRYP.ppb")
plot.rect.nodata("TRYP.ppb")
xdf$CDOM.RV = backcal.RV(parm.callog="CDOM",parm.xdf="CDOM.ugL")
plot.rect.nodata("CDOM.ugL")
xdf$Turb.Manta.RV = backcal.RV(parm.callog="TURB",parm.xdf="Turb.Manta.FNU")
plot.rect.nodata(parname.xdf="Turb.Manta.FNU")
}

# Write to file the xdf with all data, including data that will be removed, for plotting in the report.

{
  setwd(outdir.wqdl.w.alldata)
  fname.out = paste0(substr(fname,1,10),"_w_RV_all_raw_data.csv")
  write.csv(xdf,fname.out)
}

# Assign NA values to times when ALL parameters were bad (sonde dry, muddy, etc)
{
  x.qaqc.ALL = x.qaqc[x.qaqc$Parameter=="ALL",]  # Only where all parameters were bad
  xdf.out = xdf
  for (r in 1:nrow(x.qaqc.ALL)){  # For each row with ALL parameters flagged:
    x.qaqc.sub = x.qaqc.ALL[r,]
    xdf.out[(xdf.out$Date.time<=x.qaqc.sub$End.date.time) & (xdf.out$Date.time>=x.qaqc.sub$Start.date.time),seq(3,ncol(xdf.out))] = NA
  }
}

# Check to make sure it worked by adding to the time series plots
plot.ts.nona <- function(parm.xdf){
  {
    par(mar=c(1,5,1,3))
    plot(xdf$Date.time,xdf[,parm.xdf],type="l",lwd=3,col="black",las=1,ylab="") 
    mtext(side=2,parm.xdf,line=3.5)
    lines(xdf.out$Date.time,xdf.out[,parm.xdf],col="blue")
  }
}

{
  par(mfrow=c(3,1))
  plot.ts.nona(parm.xdf="TRYP.ppb")
  plot.rect.nodata("TRYP.ppb")
  plot.ts.nona(parm.xdf="CDOM.ugL")
  plot.rect.nodata("CDOM.ugL")
  plot.ts.nona(parm.xdf="Turb.Manta.FNU")
  plot.rect.nodata(parname.xdf="Turb.Manta.FNU")
}

# Write xdf file with calculated RV values
{
setwd(outdir.wqdl)
fname.out = paste0(substr(fname,1,10),"_QAQC_w_RV.csv")
write.csv(xdf.out,fname.out)
}

# Next step:  Create flags for individual parameters:  "2_interactive_qaqc_each_parm.R"
# Then new calibrations for TRYP, CDOM, TURB based on previous calibration(s)


