# Identify parameter-specific places for flags with interactive plotting.

library(plotly)

{
indir.qaqc.data = "K:/Shared drives/Project - Tijuana River Monitoring/data/sonde_IBWC_12215259/wqdatalive_downloads/3_post_QAQC/"
flist = list.files(indir.qaqc.data)
date.string = "2022_11_14"  # Date of download
fname.timeseries.qaqc = paste0(date.string,"_QAQC_w_RV.csv")
setwd(indir.qaqc.data)
xdf = read.csv(fname.timeseries.qaqc)
}

xdf$Date.time = as.POSIXlt(xdf$Date.time)

xdf = xdf[!is.na(xdf$Temp.C),] # Shorten to just where there is Temp data on the sonde--sonde always reports temperature
  # so this is equivalent to excluding rows where all parameters have NA values.

# Make summary plot of each parameter, identify parameters where closer inspection is needed.
par(mfrow=c(3,1),mar=c(1,1,1,1),oma=c(2,3,0,3))

# Plot temp and pH together
{
plot(xdf$Date.time,xdf$Temp.C,type="l",xlab="",ylab="",col="black")
mtext(side=2,line=2.5,"TempC")
par(new=TRUE)
plot(xdf$Date.time,xdf$pH,type="l",col="blue",yaxt="n",xaxt="n")
axis(side=4)
mtext(side=4,line=2.5,"pH")
legend("topright",c("Temp","pH"),lty=1,col=c("black","blue"))
}

# Plot conductivity, DO together
{
plot(xdf$Date.time,xdf$Cond.uS.cm,type="l",xlab="",ylab="",col="black")
mtext(side=2,line=2.5,"Cond uS/cm")
par(new=TRUE)
plot(xdf$Date.time,xdf$DO.mgL,type="l",col="blue",yaxt="n",xaxt="n")
axis(side=4)
mtext(side=4,line=2.5,"DO mg/L")
legend("topright",c("Cond","DO"),lty=1,col=c("black","blue"))
}

# Plot CDOM, TRYP together
{
  plot(xdf$Date.time,xdf$TRYP.ppb,type="l",xlab="",ylab="",col="black")
  mtext(side=2,line=2.5,"TRYP ppb")
  par(new=TRUE)
  plot(xdf$Date.time,xdf$CDOM.ugL,type="l",col="blue",yaxt="n",xaxt="n")
  axis(side=4)
  mtext(side=4,line=2.5,"CDOM ug/L")
  legend("topright",c("TRYP","CDOM"),lty=1,col=c("black","blue"))
}

# Plot turbidity together to get a sense of parameter ranges
par(mfrow=c(1,1))
{
  plot(xdf$Date.time,xdf$Turb.SS.TU,type="l",xlab="",ylab="",col="black")
  lines(xdf$Date.time,xdf$Turb.BS.TU,col="blue")
  mtext(side=2,line=2.5,"OBS BS or SS")
  par(new=TRUE)
  plot(xdf$Date.time,xdf$Turb.Manta.FNU,type="l",col="green",yaxt="n",xaxt="n",xlab="",ylab="")
  axis(side=4)
  mtext(side=4,line=2.5,"Manta")
  legend("topright",c("OBS BS","OBS SS","Manta"),lty=1,col=c("black","blue","green"))
}

# Plot turbidity interactively
par(mfrow=c(1,1))
# https://plotly.com/r/axes/
plot_ly(x=as.character(xdf$Date.time),y=xdf$Turb.SS.TU,type="scatter",mode="lines")
plot_ly(x=as.character(xdf$Date.time),y=xdf$Turb.BS.TU,type="scatter",mode="lines")
plot_ly(x=as.character(xdf$Date.time),y=xdf$Turb.Manta.FNU,type="scatter",mode="lines")
#  Based on plotly plots, assign QAQC (start time, end time, comments) to the csv document, save to file

###  Read in revised QAQC doc, assign flags to each parameters.
  # Load QAQC notes
{
  indir.qaqc.notes = "K:/Shared drives/Project - Tijuana River Monitoring/data/sonde_IBWC_12215259/wqdatalive_downloads/"
  flist.qaqc = list.files(indir.qaqc.notes,pattern="csv")
  qaqc.length = length(flist.qaqc)
  setwd(indir.qaqc.notes)
  x.qaqc = read.csv(flist.qaqc)
  x.qaqc$Start.date.time = strptime(x.qaqc$Start,format="%m/%d/%Y %H:%M")
  x.qaqc$End.date.time = strptime(x.qaqc$End,format="%m/%d/%Y %H:%M")
}

# Create Flag columns
  # Default Flag is "Good" G
{
xdf$Turb.Manta.Flag = "G"
xdf$Turb.BS.Flag = "G"
xdf$Turb.SS.Flag = "G"
xdf$TRYP.Flag = "G"
xdf$CDOM.Flag = "G"
xdf$Cond.Flag = "G"
}

parmlist = unique(x.qaqc$Parameter)  # List of parameters to scroll through.
parmlist = parmlist[grep("ALL",parmlist,invert=TRUE)]  # Exclude ALL, which already assigned NA values in Step 1

xdf.names = names(xdf)

# Go through each parameter in the QAQC list and assign flags
for (p in 1:length(parmlist)){
  xdf.names.sub = xdf.names[grep(parmlist[p],xdf.names)]
  xdf.names.flag = xdf.names.sub[grep("Flag",xdf.names.sub)]  # Name of the colum with the flag for this parameter
  xdf.names.sub2 = xdf.names.sub[grep("Flag",xdf.names.sub,invert=TRUE)]
  xdf.names.single = xdf.names.sub2[grep("RV",xdf.names.sub2,invert=TRUE)]
  if (length(xdf.names.single)>1){
    print("More than one column in dataset matches the QAQC parameter name.")
  } else {
    colindex = which(xdf.names==xdf.names.single)
    colindex.flag = which(xdf.names == xdf.names.flag)
  }
  # Scroll through all QAQC notes for that parameter and assign flags
  x.qaqc.sub = x.qaqc[x.qaqc$Parameter==parmlist[p],]
  for (q in 1:nrow(x.qaqc.sub)){  # Go through each entry in QAQC for that parameters
    rowindices = which((xdf$Date.time>=x.qaqc.sub$Start.date.time[q]) & (xdf$Date.time <= x.qaqc.sub$End.date.time[q]))
    for (r in 1:length(rowindices)){  # Go through each row of xdf in that parameter and QAQC entry (necessary to paste multiple flags into one flag field)
       rind = rowindices[r]
    if (xdf[rind,colindex.flag]=="G"){  # If current label is "G" then substitute new flag; else paste flags together
      xdf[rind,colindex.flag] = x.qaqc.sub[q,"Screen.label"]
    } else {
      xdf[rind,colindex.flag] = paste(xdf[rind,colindex.flag], x.qaqc.sub[q,"Screen.label"],sep=",")
      }
    }
  }
}

flag.cols = grep("Flag",xdf.names)
flag.cols = flag.cols[grep("Wet.Dry",xdf.names[flag.cols],invert=TRUE)]
qaqc.codes = unique(as.vector(as.matrix(xdf[,flag.cols]))) # https://stackoverflow.com/questions/40003028/extracting-unique-values-from-data-frame-using-r
#  Plot each series by QAQC code (e.g. dotted line for qaqc questionable).
# To double check that it did what you thought
  # Plot lines by qaqc code.  VAR (variable) gets lty=1, since not so bad.
linetype.by.qaqc = data.frame(Flag = c("CAL","CAL,RNG","G","X","VAR"),linetypes=c(2,3,1,3,5))  # 5 = longdash
pch.by.qaqc = data.frame(Flag = c("CAL","CAL,RNG","G","X","VAR"),pchval=c(1,4,NA,4,0))  


# plot time series and flag for each variable
# Plot CDOM, TRYP together
{
  linetype.tryp = merge(xdf[,c("Date.time","TRYP.ppb","TRYP.Flag")],linetype.by.qaqc,by.x="TRYP.Flag",by.y="Flag")
  pchval.tryp = merge(xdf[,c("Date.time","TRYP.ppb","TRYP.Flag")],pch.by.qaqc,by.x="TRYP.Flag",by.y="Flag")
  pchval.tryp = pchval.tryp[order(pchval.tryp$Date.time),]
  plot(xdf$Date.time,xdf$TRYP.ppb,type="l",xlab="",ylab="",col="black",lty=linetype.tryp$linetypes)
  points(xdf$Date.time,xdf$TRYP.ppb,pch=pchval.tryp$pchval,col="black")
  mtext(side=2,line=2.5,"TRYP ppb")
  
  linetype.cdom = merge(xdf[,c("Date.time","CDOM.ugL","CDOM.Flag")],linetype.by.qaqc,by.x="CDOM.Flag",by.y="Flag")
  pchval.cdom = merge(xdf[,c("Date.time","CDOM.ugL","CDOM.Flag")],pch.by.qaqc,by.x="CDOM.Flag",by.y="Flag")
  pchval.cdom = pchval.cdom[order(pchval.cdom$Date.time),]
  par(new=TRUE)
  plot(xdf$Date.time,xdf$CDOM.ugL,type="l",col="blue",yaxt="n",xaxt="n",lty=linetype.cdom$linetypes)
  points(xdf$Date.time,xdf$CDOM.ugL,pch=pchval.cdom$pchval,col="blue")
  axis(side=4)
  mtext(side=4,line=2.5,"CDOM ug/L")
  legend("topright",c("TRYP","CDOM"),lty=1,col=c("black","blue"))
}

# Plot turbidities
{
  par(xpd=T,mar=c(3,4,4,4))   # 
   # Setting margins allows for plotting legend outside the plot boundary
  xdf.ss = xdf[,c("Date.time","Turb.SS.TU","Turb.SS.Flag")]
  linetype.ss = merge(xdf.ss,linetype.by.qaqc,by.x="Turb.SS.Flag",by.y="Flag")
  pchval.ss = merge(xdf.ss,pch.by.qaqc,by.x="Turb.SS.Flag",by.y="Flag",all.x=TRUE,sort=FALSE)
  # check
  pchval.ss = pchval.ss[order(pchval.ss$Date.time),]
  
  pchval.bs = merge(xdf[,c("Date.time","Turb.BS.TU","Turb.BS.Flag")],pch.by.qaqc,by.x="Turb.BS.Flag",by.y="Flag")
  pchval.bs = pchval.bs[order(pchval.bs$Date.time),]
  
  pchval.turb.manta = merge(xdf[,c("Date.time","Turb.Manta.FNU","Turb.Manta.Flag")],pch.by.qaqc,by.x="Turb.Manta.Flag",by.y="Flag")
  pchval.turb.manta = pchval.turb.manta[order(pchval.turb.manta$Date.time),]
  
  # data.frame(xdf[,c("Turb.SS.TU","Turb.SS.Flag")],pchval.ss)  # check
  plot(xdf$Date.time,xdf$Turb.SS.TU,type="l",xlab="",ylab="",col="black",lty=1,format="%b%d %H:%M")
  points(xdf$Date.time,xdf$Turb.SS.TU,pch=pchval.ss$pchval,col="black")
  lines(xdf$Date.time,xdf$Turb.BS.TU,col="blue")
  points(xdf$Date.time,xdf$Turb.BS.TU,pch=pchval.bs$pchval,col="blue")
  mtext(side=2,line=2.5,"OBS BS or SS")
  par(new=TRUE)
  plot(xdf$Date.time,xdf$Turb.Manta.FNU,type="l",col="green",yaxt="n",xaxt="n",xlab="",ylab="")
  points(xdf$Date.time,xdf$Turb.Manta.FNU,col="green",pch=pchval.turb.manta$pchval)
  axis(side=4)
  mtext(side=4,line=2.5,"Manta")
  legend("topright",inset=c(0,-0.15),c("OBS BS","OBS SS","Manta","VAR","CAL","RNG, X"),horiz=TRUE,lty=c(1,1,1,NA,NA,NA),pch=c(NA,NA,NA,0,1,4),col=c("black","blue","green",rep("black",3)))
}

xdf = xdf[,-1]  # Remove first column--row numbers
# Write new xdf with flags to file
{
outdir.qaqc.w.flags = "K:/Shared drives/Project - Tijuana River Monitoring/data/sonde_IBWC_12215259/wqdatalive_downloads/4_with_flags/"
setwd(outdir.qaqc.w.flags)
fname.timeseries.qaqc.w.flags = paste0(date.string,"_QAQC_w_flags.csv")
write.csv(xdf,file = fname.timeseries.qaqc.w.flags,row.names=FALSE)
}

