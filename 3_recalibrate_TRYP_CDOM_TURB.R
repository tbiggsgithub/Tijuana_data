# Step 3:  Recalculate TRYP, CDOM and/or TURB with other calibration parameters.

# Read in qaqc data that has flags
{
indir.qaqc.w.flags = "K:/Shared drives/Project - Tijuana River Monitoring/data/sonde_IBWC_12215259/wqdatalive_downloads/4_with_flags/"
date.string = "2022_11_14"  # Date of download
fname.timeseries.qaqc.w.flags = paste0(date.string,"_QAQC_w_flags.csv")

setwd(indir.qaqc.w.flags)
xdf = read.csv(fname.timeseries.qaqc.w.flags)
}

# Determine calibration parameters for TRYP, CDOM, TURB
  # Note:  parameters of 0, 1 mean just use the current value



