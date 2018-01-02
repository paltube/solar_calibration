#!/usr/bin/env Rscript

# Program in R - Select solar observations (Sun proximity criterion & outlier removal)  
# P. Altube Vazquez - July 2017

#########################################################################################
##### INFO and USAGE ####################################################################

# This script has been developed for its automatic and daily execution within the solar 
# monitoring project (SunINTFCal). The shell script that boots up the daily proccess is: 
# "SunINTFCal_daily.sh"
#                                                                         
# This script loads constant and continuous interference data (input file: INTF_YYMMDD_XXX.txt) 
# and  selects solar interferences among them. Solar interferences are recorded in a 
# date-identified file for each radar: "SunINTF_YYMMDD_XXX.txt".
#
# Command line execution: specify date and radar abbrv., work path and settings file.
# Rscript /.../Scripts/b_SunINTF_detector.R YYMMDD XXX /... settings.ini 

#########################################################################################
##### LIBRARIES #########################################################################

options(warn=-1)

library(getopt)

time_now <- Sys.time()

#########################################################################################
##  FUNCTIONS  ##########################################################################

# Extract numerical values from input file header
read_num_header <- function(hdr_line){
  num <- gsub("^(([^:]+):)","", hdr_line)
  num <- gsub("[A-Z]","", num)
  num <- gsub("[a-z]","", num)
  num <- as.numeric(num)
  return(num)
}

# Error function for power correction and scaling factors' calculation
erf <- function(x){
  y <- 2*pnorm(sqrt(2)*x)-1
  return(y)
}

# Ray-sun misalignment dependent power correction (with a third order aprox. for 
# a cosine dependent integral)
scan_corr_factor <- function(d_beam, d_sun, d_radial, d_el0, d_az0){
  
  S <- (4/pi)*(1/(d_sun^2))
  C0 <- 4*log(2)*((d_sun/d_beam)^2)
  L0 <- (4/C0)*(1 - exp(-(C0/4)))
  L2 <- (pi*S)*(1 - exp(-(C0/4))*(1 + (C0/4)))
  D <- C0/(d_sun^2)
  
  lim1 <- d_az0 - (d_radial/2)
  lim2 <- d_az0 + (d_radial/2)
  C1 <- exp(-D*(d_el0^2))
  E1 <- erf(sqrt(D)*lim1)
  E2 <- erf(sqrt(D)*lim2)
  TB1 <- lim1*exp(-D*(lim1^2))
  TB2 <- lim2*exp(-D*(lim2^2))
  
  A <- (1/2)*(1/d_radial)*sqrt(pi/D)*C1*(L0 + L2*((d_el0^2) + (1/(2*D))))
  B <- (1/2)*(1/d_radial)*C1*(L2/D)
  
  corr <- A*(E2 - E1) + B*(TB1 - TB2)
  return(corr)
  
}

# Estimate optical path length of the solar energy through the atmosphere
path_length <- function(sun_el, k, re, z, hmasl){
  
  a <- z + hmasl
  b <- sin(sun_el*pi/180)
  radius_eff <- k*re
  
  r <- radius_eff*(sqrt(b^2 + (a/radius_eff)^2 + 2*(a/radius_eff)) - b)
  
  return(r)
  
}

#########################################################################################
## DEFAULT SETTINGS #####################################################################

# Date, radar abbrv. and input .tgz file if not provided as command line arguments:
date_str <- "170730"
radar <- "PBE"

work_path <- "/home/operator/progR/SunINTFCal"
ini_file <- paste(work_path, "SunINTFCal.ini", sep="/")

k <- 5/4 # k-model constant
z0 <- 8.4 # Equivalent height of constant density atmosphere [km]
radius_earth <- 6378 # Earth radius [km]
a <- 0.016 # Atm. att. coeff. (two-way) [dB/km]
dx_max <- 5 # Max. allowed (absolute) distance from Sun in azimuth [deg]
dy_max <- 5 # Max. allowed (absolute) distance from Sun in elevation [deg]
d_sun <- 0.57 # Sun-disk apparent diameter [deg]
sigma_max <- 2 # Max. number of sigma-intervals allowed for outlier removal

#########################################################################################
# SETTINGS #############################################################################

# Number of header lines in the input file:
hdr_length <- 15

#########################################################################################
## ARGUMENTS ############################################################################

#Retrieve command-line argument(s): Date and radar name
options <- commandArgs(trailingOnly=T)

if (!is.na(options[1])){
  date_str <- options[1]
}
date_obj <- as.Date(date_str, format="%y%m%d")

if (!is.na(options[2])){
  radar <- options[2]
}

if (!is.na(options[3])){
  work_path <- options[3]
}

# LOG file:
log_path <- paste(work_path, radar, "LOG/", sep="/")
out_file_log <- paste(log_path, "LOGsunINTF_", date_str, "_", radar, ".txt", sep="")
if (!file.exists(log_path)){dir.create(log_path, showWarnings =F, recursive = T)}
cat("\n# EXECUTION: b_SunINTF_detector.R\n  ", as.character(time_now), 
    "\n", sep="", file=out_file_log, append=T)
cat("\n# EXECUTION: b_SunINTF_detector.R ", as.character(time_now), "\n", sep="")

if (!is.na(options[4])){
  
  ini_file <- options[4]
  cat("  # Settings from: ", ini_file, "\n", file=out_file_log, sep="", append=T)
  
}else{
  cat("  # No settings file specified, default values will be used\n", file=out_file_log, sep="", append=T)
}

# Define outpath based on directory tree structure and build it if not already:
out_path <- paste(work_path, radar, "Rec/", sep="/")
if (!file.exists(out_path)){dir.create(out_path, showWarnings =F, recursive = T)}

if (file.exists(ini_file)) {
  
  # Open connection
  ini_conn <- file(ini_file, "r")
  ini_params <- readLines(ini_conn)
  
  k <- read_num_header(ini_params[grep("k-model", ini_params)])
  z0 <- read_num_header(ini_params[grep("Equivalent height", ini_params)])
  radius_earth <- read_num_header(ini_params[grep("Earth radius", ini_params)])
  a <- read_num_header(ini_params[grep("Attenuation", ini_params)])
  dx_max <- read_num_header(ini_params[grep("Max. AZ distance", ini_params)])
  dy_max <- read_num_header(ini_params[grep("Max. EL distance", ini_params)])
  d_sun <- read_num_header(ini_params[grep("Sun-disk diameter", ini_params)])
  sigma_max <- read_num_header(ini_params[grep("sigma-intervals", ini_params)])
  
  close(ini_conn)
  rm(ini_conn)  
  
}else{
  cat("    Settings file not found, default values will be used\n", file=out_file_log, sep="", append=T)
}

#########################################################################################
## VARIABLES ############################################################################

# Output file where information of selected sun interferences is to be recorded
out_file <- paste(out_path, "SunINTF_", date_str, "_", radar, ".txt", sep="")
in_file <- paste(out_path, "INTF_", date_str, "_", radar, ".txt", sep="")
cat("  # INPUT:\n    ", in_file, file=out_file_log, sep="", append=T)

#########################################################################################
#####  SCRIPT CORE  #####################################################################
#########################################################################################

if (file.exists(in_file)){

  cat("    FILE OK\n", file=out_file_log, append=T)
  
  # Open connection
  conn <- file(in_file, "r")
  
  # Retrieve header info
  hdr <- readLines(conn, n=hdr_length)
  
  radar_hmasl <- read_num_header(hdr[grep("Radar height", hdr)])
  radar_wl <- read_num_header(hdr[grep("Radar wavelength", hdr)])
  radar_cnt <- read_num_header(hdr[grep("Radar cnt", hdr)])
  radar_zcal <- read_num_header(hdr[grep("Calib. cnt", hdr)])
  rec_bw <- read_num_header(hdr[grep("Receiver bandwidth", hdr)])
  d_beam_h <- read_num_header(hdr[grep("Beam width H", hdr)])
  d_beam_v <- read_num_header(hdr[grep("Beam width V", hdr)])
  d_radial <- read_num_header(hdr[grep("Radial resolution", hdr)])
  
  close(conn)
  rm(conn)
  
  # Calculate power scaling factor
  l_over <- (1/(log(2)))*((d_beam_h/d_sun)^2)*(1-exp(-log(2)*((d_sun/d_beam_h)^2)))
  l_avg <- (d_beam_h/d_radial)*sqrt(pi/(4*log(2)))*erf(sqrt(log(2))*(d_radial/d_beam_h))
  f_scan_dB <- 10*log10(l_over*l_avg)
  
  
  #####  READ INPUT FILE  #################################################################
  
  # Initialise output data frame
  data_solar <- data.frame(matrix(ncol = 16, nrow = 0))
  colnames(data_solar) <- c("date", "time", "intf_time", "d_intf", "az_r",
                            "el_r", "az_s", "el_s", "pow", "sd_pow", "daz_x",
                            "del_y", "l_gas", "l_scan", "pow_corr", "label")
  
  line_num <- length(readLines(in_file))
  if (line_num > (hdr_length+1)){
    
    data_tmp <- read.table(file=in_file, sep= " ", header=T, skip=hdr_length)
    
    data_tmp <- data_tmp[data_tmp$label=="constant",]
    data_tmp <- subset(data_tmp, select=-c(label))
    
    data_tmp$date <- as.Date(data_tmp$date)
    
    data_tmp$az_r <- (360 + data_tmp$az_r)%%360
    
    # Delete data rows containing NA (just in case):
    data_solar <- data_tmp[complete.cases(data_tmp),]
    
    #####  IDENTIFICATION of solar interferences  ###########################################
    
    # Calculate antenna-Sun zenithal and azimuthal misalignments
    data_solar$daz_x <- data_solar$az_r - data_solar$az_s
    data_solar$del_y <- data_solar$el_r - data_solar$el_s
    
    data_solar[data_solar$daz_x < -180, "daz_x"] <- -(360 - data_solar[data_solar$daz_x < -180, "daz_x"])%%360
    data_solar[data_solar$daz_x > 180, "daz_x"] <- 360 - data_solar[data_solar$daz_x > 180, "daz_x"]
    
    # Select Sun interferences by proximity to Sun position as specified in SETTINGS:
    data_solar <-  data_solar[data_solar$del_y>-dy_max & data_solar$del_y<dy_max,]
    data_solar <-  data_solar[data_solar$daz_x>-dx_max & data_solar$daz_x<dx_max,]
    
    data_solar$del_y <- round(data_solar$del_y, 2)
    data_solar$daz_x <- round(data_solar$daz_x, 2)
    
    #####  POWER CORRECTION  ################################################################
    
    # Gaseous atmospheric attenuation losses [dBm]
    data_solar$l_gas <- (a/2)*path_length(data_solar$el_s, k, radius_earth, z0, radar_hmasl/1000)
    data_solar$l_gas <- round(data_solar$l_gas, 2)
    
    # Antenna scanning losses:
    data_solar$L <- scan_corr_factor(d_beam=d_beam_h, d_sun=d_sun, d_radial=data_solar$d_intf, 
                            d_el0=data_solar$del_y, d_az0=data_solar$daz_x)
    data_solar$l_scan <- -10*log10(data_solar$L)
    data_solar$l_scan <- round(data_solar$l_scan, 2)
    
    # Power correction:
    data_solar$pow_corr <- data_solar$pow + data_solar$l_gas + data_solar$l_scan
    data_solar$pow_corr <- round(data_solar$pow_corr, 2)
    
    med_pow_corr <- median(data_solar$pow_corr, na.rm=TRUE)
    mad_pow_corr <- mad(data_solar$pow_corr, na.rm=TRUE)
    
    data_solar$label <- "solar"
    data_solar$label[data_solar$pow_corr > (med_pow_corr + sigma_max*mad_pow_corr)] <- "outlier"
    data_solar$label[data_solar$pow_corr < (med_pow_corr - sigma_max*mad_pow_corr)] <- "outlier"
 
    #####  RECORDING solar interferences  ###################################################
    
    data_solar <- subset(data_solar, select = -c(bin_frac, L))  
    # data_solar <- data_solar[complete.cases(data_solar),]
    
    if (nrow(data_solar)==0){
      # output warning to LOG file if not any interference fullfills 
      # solar interference criteria
      cat("    NO SOLAR INTERFERENCES detected for ", format(as.date_str(date_str, format="%y%m%d"), format="%Y-%m-%d"),
          "\n", file=out_file_log, sep="", append=T)
      
    }else{
      
      # Format data frame values
      data_solar[, c(2,3)] <- sprintf("%.4f", round(unlist(data_solar[, c(2,3)]),4))
      data_solar[, 4:15] <- sprintf("%.2f", round(unlist(data_solar[, 4:15]),2))
      
    }
    
  }else{
    # output warning to LOG file if not any interference found in input file
    cat("    WARN: NO INPUT DATA FOUND\n", sep="", file=out_file_log, append=T)
    
  }
  
  cat("  # OUTPUT: ", out_file, "\n", file=out_file_log, sep="", append=T)
  
  # Write headers in output file
  if (!file.exists(out_file)) {
    
    H1 <- "# SOLAR INTERFERENCES"
    H2 <- paste("# File created:", time_now, sep=" ")
    H3 <- paste("# Input file:", in_file, sep=" ")
    H4 <- paste("# Initialization file:", ini_file, sep=" ")
    H5 <- paste("# Radar abbrv.:", radar, sep=" ")
    H6 <- paste("# Radar height:", radar_hmasl, "masl", sep=" ")
    H7 <- paste("# Radar wavelength:", radar_wl, "cm", sep=" ")
    H8 <- paste("# Radar cnt:", radar_cnt, "dBZ", sep=" ")
    H9 <- paste("# Calib. cnt:", radar_zcal, "dBm", sep=" ")
    H10 <- paste("# Receiver bandwidth:", rec_bw, "kHz", sep=" ")
    H11 <- paste("# Beam width H:", d_beam_h, "deg", sep=" ")
    H12 <- paste("# Beam width V:", d_beam_v, "deg", sep=" ")
    H13 <- paste("# Radial resolution (nominal):", d_radial, "deg", sep=" ")
    H14 <- paste("# Power scaling factor:", round(f_scan_dB,2), "dB", sep=" ")
    
    header <- c(H1, H2, H3, H4, H5, H6, H7, H8, H9, H10, H11, H12, H13, H14, "#")
    write(header, file=out_file, ncolumns=15, sep="\n", append=T)
    
    write.table(data_solar, file=out_file, append =T, quote =F, sep = " ", row.names = F, col.names=T)
    
  }else{
    
    cat("    WARN: There is already an output file with the specified name\n", sep="", file=out_file_log, append=T)
    cat("          Please, erase it and run again for current results registration\n", sep="", file=out_file_log, append=T)
  }
  
}else{
  # output warning to LOG file if input file not found (e.g. when initial daily .tgz 
  # file was not available for execution of the precedent "RawINTF.R" script or when such
  # script has not worked properly)
  cat("    FILE NOT FOUND\n", sep="", file=out_file_log, append=T)
  cat("    WARN: NO OUTPUT FILE GENERATED\n", sep="", file=out_file_log, append=T)
}

#########################################################################################
## EOS ##
#########################################################################################
