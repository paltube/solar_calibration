#!/usr/bin/env Rscript

# Program in R - Monthly accumulations of non-solar interferences
# P. Altube Vazquez - July 2017

#########################################################################################
##### INFO and USAGE ####################################################################



#########################################################################################
##### LIBRARIES #########################################################################

library(raster, quietly=T)
library(akima, quietly=T) # interpolation
library(ggplot2, quietly=T) # plotting
library(GISTools, quietly=T)

time_now <- Sys.time()

#########################################################################################
## DEFAULT SETTINGS #####################################################################

work_path <- "/home/operator/progR/SunINTFCal"
#work_path <- "/home/pav/Desktop/WIFIopera"

radar <- "PBE"

# Start and end dates of the analysis
date_str_i <- "170701"
date_str_f <- "170731"

# Boolean that indicates whether the date info is included in the name of output figures
date_bool <- T

# Shapefiles of Catalonia region
shpfile_com_name <- "comarca.shp"
shpfile_mun_name <- "munis.shp"

# Files containing location of Tradia telecommunication stations
statfile_trl_name <- "TRADIA_TRONCAL.txt"
statfile_nod_name <- "TRADIA_NODAL.txt"

#########################################################################################
## FUNCTIONS ############################################################################

plot_labels <- function(major_breaks, minor_breaks, sig_dig=2, ndec=1){
  
  minor_breaks <- round(minor_breaks, ndec) 
  major_breaks <- round(major_breaks, ndec)  
  
  minor_breaks <- format(minor_breaks, digits=sig_dig, nsmall=ndec)
  w <- max(nchar(minor_breaks))
  major_breaks <- format(major_breaks, digits=sig_dig, nsmall=ndec, width=w)  
  
  labels <- rep("",length(minor_breaks))
  for (m in major_breaks){
    labels[which(minor_breaks==m)] <- m
  }
  return(labels)
}

#########################################################################################
# SETTINGS #############################################################################

el_max <- 0.7 # Maximum elevation considered
scan_num <- 240 # Number of sweeps per day corresponding to the max elevation
myproj <- "+proj=utm +zone=31 ellps=WGS84" # Projection for shapefiles and raster

#########################################################################################
## PARAMETERS ###########################################################################

# Number of header lines in input file
hdr_lines <- 15
# Max. range for raster (somewhat arbitrary)
range_max <- 240
# Breakpoints of azimuthal units for histogram
breakpts <- seq(-0.5, 359.5)

# Radar UTM coordinates
xrad_xutm <- list("CDV"=366657.02 , "PDA"=499170.35, "LMI"=320253.82 , "PBE"=406329.96)
xrad_yutm <- list("CDV"=4606683.3 , "PDA"=4637645.3, "LMI"=4551034.1 , "PBE"=4580517.1)

# Customised colorbar
colbreaks <- c(0, 0.5, 1, 2, 5, 10, 20, 50, 100)
colbreaks_at <- seq(0, 100, 12.5)
colscale <- c("white", "ivory2", "lightgoldenrodyellow", "lightgoldenrod1", "gold2", "darkorange2", "orangered3", "black")
colscale_alpha <- adjustcolor(colscale, alpha.f=0.7)

# Extent of raster to be plotted
x_min_r <- 250000
x_max_r <- 550000
y_min_r <- 4450000
y_max_r <- 4750000

# Histogram limits:
y_min_h <- 0
y_max_h <- 25
step_h <- 5

#########################################################################################
## ARGUMENTS ############################################################################

#Retrieve command-line argument(s): Date and radar name
options <- commandArgs(trailingOnly=T)

if ((length(options)==4)|(length(options)==5)){
  date_str_i <- options[1]
  date_str_f <- options[2]
  radar <- options[3]
  work_path <- options[4]
}

if (length(options)==5){
  if (options[5]=="NODATE"){
    date_bool <- F
  }else{
    date_bool <- T
  }
}

#########################################################################################
## VARIABLES ############################################################################

# Date formatting
date_obj_i <- as.Date(date_str_i, format="%y%m%d")
date_obj_f <- as.Date(date_str_f, format="%y%m%d")

ym <- format(date_obj_f, format="%y%m")
ym_tit <- format(date_obj_f, format="%b %Y")

month <- format(date_obj_f, format="%m")

date_obj_seq <- seq(date_obj_i, date_obj_f, 1)
date_str_seq <- format(date_obj_seq, format="%y%m%d")

# Shapefiles of Catalonia region
shpfile_com <- paste(work_path, "/SHP/", shpfile_com_name, sep="")
shpfile_mun <- paste(work_path, "/SHP/", shpfile_mun_name, sep="")

# Files containing location of Tradia telecommunication stations
statfile_trl <- paste(work_path, "/Tradia/", statfile_trl_name, sep="")
statfile_nod <- paste(work_path, "/Tradia/", statfile_nod_name, sep="")

# LOG file:
log_path <- paste(work_path, radar, "LOG/", sep="/")
out_file_log <- paste(log_path, "LOGsunINTF_", date_str_f, "_", radar, ".txt", sep="")
if (!file.exists(log_path)){dir.create(log_path, showWarnings =F, recursive = T)}

# Input and Output paths
in_path <- paste(work_path, radar, "Rec", sep="/")
out_path <- paste(work_path, radar, "Emitter", sep="/")
if (!file.exists(out_path)){dir.create(log_path, showWarnings =F, recursive = T)}

# Title for output figures
tit <- paste("External interferences:", radar, ym_tit)

## OUTPUT ###########################################################################

if (!file.exists(out_path)){
  dir.create(out_path, recursive=T)
}

# Output data files
out_hist_file <- paste(out_path, "/INTFhist_", ym, "_", radar, ".txt", sep="" )
out_tiff_file <- paste(out_path, "/INTFrast_", ym, "_", radar, ".tiff", sep="" )

# Output figures
if (date_bool){
  out_hist_plot <- paste(out_path, "/INTFhist_", ym, "_", radar, ".png", sep="" )
  out_tiff_plot <- paste(out_path, "/INTFrast_", ym, "_", radar, ".png", sep="" )
  
}else{
  out_hist_plot <- paste(out_path, "/INTFhist_", radar, ".png", sep="" )
  out_tiff_plot <- paste(out_path, "/INTFrast_", radar, ".png", sep="" )
  
}

#########################################################################################
## CORE #################################################################################

# LOG file: execution message
cat("\n# INTERFERENCES FROM EXTERNAL EMITTERS: monthly/semestral analysis\n",
    file=out_file_log, append=T, sep="")
cat("\n# EXECUTION: INTFopera.R\n", "  ", as.character(time_now), "\n",
    file=out_file_log, append=T, sep="")
cat("\n# EXECUTION: INTFopera.R ", as.character(time_now), "\n", sep="")

# Read shapefiles  
shp_com <- readShapeLines(shpfile_com, proj4string = CRS(myproj))
shp_mun <- readShapeLines(shpfile_mun, proj4string = CRS(myproj))

# Read Tradia stations' files
nodal_stations <- read.table(statfile_nod, header = T)
troncal_stations <- read.table(statfile_trl, header = T)

## INPUT DATA ###########################################################################

cat("    INPUT:\n", file=out_file_log, sep="", append=T)

# Input files
in_files_all <- c()
in_files_sun <- c()
for (i in seq(1, length(date_str_seq))){
  
  dd <- date_str_seq[i]
  in_files_all <- c(in_files_all, paste(in_path, "/INTF_", dd, "_", radar, ".txt", sep=""))
  in_files_sun <- c(in_files_sun, paste(in_path, "/SunINTF_", dd, "_", radar, ".txt", sep=""))
}

in_files_all <- in_files_all[file.exists(in_files_all)]
in_files_sun <- in_files_sun[file.exists(in_files_all)]
date_str_seq <- date_str_seq[file.exists(in_files_all)]

# Generate output only if any input files exist
if (length(in_files_all)!=0){
  
  data_intf <- data.frame()
  for (i in seq(1, length(in_files_all))){
    
    dd <- date_str_seq[i]
    
    in_file_all <- in_files_all[i]
    in_file_sun <- in_files_sun[i]
    
    cat("    ", in_file_all, file=out_file_log, sep="", append=T)
    
      line_num <- length(readLines(in_file_all))
      if (line_num > (hdr_lines +1)){
        
        cat("    FILE OK", "\n", file=out_file_log, append=T, sep="")
        
        data_all_tmp <- read.table(file=in_file_all, sep= " ", header=T, skip=hdr_lines)
        data_all_tmp <- data_all_tmp[data_all_tmp$label=="constant",]
        data_all_tmp <- data_all_tmp[data_all_tmp$el_r<=el_max,]
        data_all_tmp$date <- as.Date(data_all_tmp$date)
        
        # Keep non-solar observations (comparison of all intfs. with solar intfs.)
        if (file.exists(in_file_sun)){
          line_num_sun <- length(readLines(in_file_sun))
          if (line_num_sun > (hdr_lines +1)){
            data_all_sun <- read.table(file=in_file_sun, sep= " ", header=T, skip=hdr_lines)
            times_sun <- data_all_sun$intf_time
            data_all_tmp <- data_all_tmp[!(data_all_tmp$intf_time %in% times_sun), ]
          }
        }
        
        data_all_tmp <- subset(data_all_tmp, select=c(date, intf_time, az_r, el_r, pow))
        data_intf <- rbind(data_intf, data_all_tmp)
        
      }else{
        cat("    NO DATA IN FILE", "\n", file=out_file_log, append=T, sep="")
      }
    
  }
  
  # Number of interferences
  int_num <- nrow(data_intf)
  # Number of sweeps considered
  scan_num_tot <- scan_num*length(unique(data_intf$date))
  
  cat("    OUTPUT:\n", file=out_file_log, sep="", append=T)
  
  ## FREQUENCY COUNTS ###################################################################
  
  if (int_num!=0){
    
    # Assign interferences to corresponding 'whole' azimuth position
    data_intf$az_r <- round(data_intf$az_r, 0)
    data_intf$az_r[data_intf$az_r>359.5] <- 0
    
    # Histogram
    h <- hist(data_intf$az_r, breaks=breakpts, plot=F)
    
    # Histogram breaks
    az_i <- h$mids - 0.5
    az_f <- h$mids + 0.5
    az_m <- h$mids
    
    # Convert frequency counts to fraction of affected sweeps
    counts_frac <- h$counts/scan_num_tot
    percent <- round(100*counts_frac,2)
    freq <- h$counts
    
  }else{
    
    az_i <- seq(-0.5, 358.5)
    az_f <- seq(0.5, 359.5)
    az_m <- (az_f+az_i)/2
    percent <- rep(0, length(az_i))
    freq <- rep(0, length(az_i))
    
  }
  
  # Data frame with incidence data
  data_hist <-  data.frame(radar=radar, az_i=az_i, az_f=az_f, az_m=az_m, 
                           percent=percent, freq=freq)
  
  # Write histogram data to output file
  write.table(data_hist, file=out_hist_file, append=F, quote=F, row.names=F, col.names=T)
  
  cat("    ", out_hist_file,  "\n", file=out_file_log, sep="", append=T)
  
  ## RASTER & TIFF ########################################################################
  
  # Matrix building for raster
  mat_freq <- matrix(rep(data_hist$percent, range_max), nrow=nrow(data_hist), ncol=range_max)
  mat_azs <- matrix(rep(data_hist$az_m, range_max), nrow=nrow(data_hist), ncol=range_max)
  mat_ranges <- t(matrix(rep(seq(0.5,(range_max-0.5)), nrow(data_hist)), 
                         nrow=range_max, ncol=nrow(data_hist)))*1000
  
  mat_xutm <- xrad_xutm[[radar]] + mat_ranges*sin(mat_azs*pi/180)
  mat_yutm <- xrad_yutm[[radar]] + mat_ranges*cos(mat_azs*pi/180)
  
  xutm_max <- xrad_xutm[[radar]] + range_max*1000 + 500
  xutm_min <- xrad_xutm[[radar]] - range_max*1000 - 500
  yutm_max <- xrad_yutm[[radar]] + range_max*1000 + 500
  yutm_min <- xrad_yutm[[radar]] - range_max*1000 - 500
  
  xutm_grid <- seq(xutm_min, xutm_max, 500)
  yutm_grid <- seq(yutm_min, yutm_max, 500)
  
  # Interpolate frequency/percentage data to rectangular utm grid
  freq_interp <- interp(x=as.vector(mat_xutm), y=as.vector(mat_yutm), 
                        z=as.vector(mat_freq), 
                        xo=xutm_grid, yo=yutm_grid, linear=TRUE, extrap=FALSE)
  
  freq_interp$z <- apply(t(freq_interp$z), 2, rev)
  
  # Build raster and write to output tif file
  rast <- raster(freq_interp$z, xmn=min(freq_interp$x), xmx=max(freq_interp$x), 
                 ymn=min(freq_interp$y), ymx=max(freq_interp$y), 
                 CRS(myproj))
  writeRaster(rast, filename=out_tiff_file, format="GTiff", overwrite=TRUE)
  
  cat("    ", out_hist_file,  "\n", file=out_file_log, sep="", append=T)
  
  # Extend raster
  ext <- extent(x_min_r, x_max_r, y_min_r, y_max_r)   
  rast_ext <- extend(rast, ext)
  
  # Position of colorscale labels
  if (int_num==0){
    
    # Legend coordinates' limits refer to c(1e-3, -1e-3) limits
    step_r <- (1e-3-(-1e-3))/(length(colscale)-1)
    labs_at <- c(-1e-3, seq(-1e-3+step_r/2, 1e-3 - step_r/2, step_r), 1e-3)
    
  }else{
    
    # Position of colorscale labels
    # Legend coordinates refer to c(0, maxValue(raster)) limits
    step_r <- (maxValue(rast_ext)-minValue(rast_ext))/(length(colscale)-1)
    labs_at <- c(0, seq(step_r/2, maxValue(rast_ext) - step_r/2, step_r), maxValue(rast_ext))
    
  }
  
  #########################################################################################
  ## RASTER PLOT ##########################################################################
  
  png(width=1500, height=1500, file=out_tiff_plot)
  par(mar=c(5,5,4,3))
  plot(rast_ext, breaks=colbreaks, col=colscale,
       xlab="UTMx [m]", ylab="UTMy [m]", cex.lab=2,
       xlim=c(x_min_r, x_max_r), ylim=c(y_min_r, y_max_r), xaxt="n", yaxt="n", legend=FALSE)
  plot(rast_ext, col=colscale, horizontal=TRUE, legend.mar=10, legend.shrink=0.8, legend.only=T, legend.width=3,
       legend.args=list(text="Affected PPIs [%]", side=1, cex=3, line=-5, las=1),
       axis.args=list(at=labs_at, labels=colbreaks, cex.axis=2.5))
  title(tit, cex.main=2.5)
  plot(shp_mun, bg="transparent", add=T, col="darkgrey")
  plot(shp_com, bg="transparent", add=T, col="black")
  points(nodal_stations$UTM_X, nodal_stations$UTM_Y, pch=16, col="dodgerblue2", cex=1.7)
  points(troncal_stations$UTM_X, troncal_stations$UTM_Y, pch=16, col="dodgerblue4", cex=1.7)
  text(x=troncal_stations$UTM_X, y=troncal_stations$UTM_Y+5000, labels=substr(troncal_stations$NOM,1,4), cex=1.5, col="dodgerblue4")
  text(x=xrad_xutm[[radar]]-10000, y=xrad_yutm[[radar]], labels=radar, cex=2.5,col="black")
  points(xrad_xutm[[radar]], xrad_yutm[[radar]], pch="*", col="black", cex=4)
  axis(1, at=seq(x_min_r+50000, x_max_r-50000, 50000), cex.axis=2)
  axis(2, at=seq(y_min_r+50000, y_max_r-50000, 50000), cex.axis=2)
  dev.off()
  
  #########################################################################################
  ## BARPLOT ##############################################################################
  
  p <- ggplot(data_hist, aes(y=percent, x=az_m)) + 
    geom_bar(stat = "identity", fill="grey55", colour="grey55") +
    scale_x_continuous(breaks=seq(10, 350, 10),
                       labels=plot_labels(seq(30, 330, 30), seq(10, 350, 10), ndec=0)) +
    scale_y_continuous(breaks=seq(y_min_h, y_max_h, step_h/2),
                       labels=plot_labels(seq(y_min_h, y_max_h, step_h), 
                                          seq(y_min_h, y_max_h, step_h/2),
                                          ndec=0)) +
    ggtitle(tit) +
    coord_cartesian(ylim=c(y_min_h, y_max_h), xlim=c(0,360)) + theme_bw() +
    xlab("Azimuth from North [deg]") + ylab("Affected PPIs [%]") + 
    theme(axis.text=element_text(size=20, colour="grey25"),
          axis.text.x=element_text(angle=0, margin=margin(c(10,10,10,10))),
          axis.text.y=element_text(margin=margin(c(10,10,10,10))),
          axis.title.y=element_text(size=20, vjust=0.3, colour="grey25", margin=margin(c(10,10,10,10))),
          axis.title.x=element_text(size=20, colour="grey25"),
          axis.ticks=element_line(colour="grey25"),
          axis.ticks.length = unit(-0.12, "cm"),
          plot.title = element_text(size=22, hjust=0.5, face = "bold"),
          panel.border = element_rect(size=0.8, colour="black"),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          panel.background = element_rect(fill = "transparent"),
          plot.background = element_rect(fill = "transparent",colour = NA),
          legend.position="none")
  
  png(file=out_hist_plot, width=1200, height=600)
  print(p)
  dev.off()
  
  cat("    ", out_tiff_plot,  "\n", file=out_file_log, sep="", append=T)
  cat("    ", out_hist_plot,  "\n", file=out_file_log, sep="", append=T)
  
}else{
  
  cat("    No input files found for the selected time period\n", file=out_file_log, sep="", append=T)
  cat("    NO OUTPUT GENERATED\n", file=out_file_log, sep="", append=T)
  
  
}

#########################################################################################
## EOS ##################################################################################