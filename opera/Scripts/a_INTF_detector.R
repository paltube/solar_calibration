#!/usr/bin/env Rscript

# Program in R - Detect constant and continuous interferences in RAW IRIS files
# P. Altube Vazquez - July 2017

#########################################################################################
##### INFO and USAGE ####################################################################

# This script has been developed for its automatic and daily execution within the solar 
# monitoring project (SunINTFCal). The shell script that boots up the daily proccess is: 
# "SunINTFCal_daily.sh"
# 
# This script parses IRIS raw files (for a particular date -YYMMDD- and radar -XXX-)
# and searchs for constant continuous interferences in polar data. For each detected 
# interference the corresponding solar position is computed. Detected interferences are
# recorded in a date-identified file for each particular radar: "INTF_YYMMDD_XXX.txt".
#
# Command line execution: specify date, radar abbrv., daily raw .tgz file name 
# (without work path), work path and settings file path+filename
# Rscript .../a_INTF_detector.R YYMMDD XXX XXXRAWYYYYMMDD.tgz ... .../settings.ini
#
# NOTE: The working directory must contain a /tmp/ folder with the required RAW files

#########################################################################################
##### LIBRARIES #########################################################################

options(warn=-1)

library(mmap)
library(TTR)
library(sfsmisc)
library(getopt)

time_now <- Sys.time()

#########################################################################################
##### FUNCTIONS #########################################################################

# Extract numerical values from .ini file
read_num_hdr <- function(hdr_line){
  num <- gsub("^(([^:]+):)","", hdr_line)
  num <- gsub("[A-Z]","", num)
  num <- gsub("[a-z]","", num)
  num <- as.numeric(num)
  return(num)
}

# Function to read bit sequence within bytes:                       
read_bits <- function(num, Nbits){
  
  seq <- digitsBase(num, base=2, ndigits=Nbits)
  indx_ones <- sort(Nbits - which(seq != 0))
  
  count <- 1
  indx_list <- list()
  for (i in indx_ones){
    indx_list[count] <- as.character(i)
    count <- count+1
  }
  
  return(indx_list)
  
}

# Function to read data/time bytes in the header and return formatted:
time <- function(data, st_byte){
  
  hdr_data <- list()
  
  secs <- readBin(data[st_byte:(st_byte+3)], integer(), size=4, n=1, signed=F)
  year <- readBin(data[(st_byte+6):(st_byte+7)], integer(), size=2, n=1, signed=T)
  month <- readBin(data[(st_byte+8):(st_byte+9)], integer(), size=2, n=1, signed=T) 
  day <- readBin(data[(st_byte+10):(st_byte+11)], integer(), size=2, n=1, signed=T)
  
  s2 <- secs%%60
  m <- (secs%/%60)%%60
  h <- (secs%/%60)%/%60
  
  # Formatted time and date info:
  if (s2 < 10){SS <- paste("0", as.character(s2), sep="")} else {SS <- as.character(s2)}
  if (m < 10){QQ <- paste("0", as.character(m), sep="")} else {QQ <- as.character(m)}
  if (h < 10){HH <- paste("0", as.character(h), sep="")} else {HH <- as.character(h)}
  if (month < 10){MM <- paste("0", as.character(month), sep="")} else {MM <- as.character(month)}
  if (day < 10){DD <- paste("0", as.character(day), sep="")} else {DD <- as.character(day)}
  
  hdr_data[["f_date"]] <- paste(as.character(year), MM, DD, sep="/")
  hdr_data[["f_time"]] <- paste(HH, QQ, SS, sep=":")
  
  return(hdr_data)
}

# Function to calculate the day number since 1st January 2000 (12:00 time_utc):
ref_day <- function(date_obj, time_utc){
  year <- as.integer(format(date_obj, format="%Y"))
  day_of_year <- as.integer(format(date_obj, format="%j"))
  # n is the number of days since 12UT of 1st January, 2000
  n <- day_of_year + (year -2000)*365 + ceiling((year - 2000)/4) - 1 + time_utc/24 - 0.5
  # Use function "ceiling" because 2000 was leap year; 
  # One day subtracted because 1st January 2000 (12UT) corresponds to n=0
  # -0.5 is the offset in days between 0UT and 12UT
  return(n)
}

# Solar elevation and azimuth (local), atm. refraction not considered:
sun_pos_fn <- function(lat, lon, date_obj, time_utc){
  
  n <- ref_day(date_obj, time_utc)
  
  l <- (280.460 + 0.9856474*n)%%360
  g <- (357.528 + 0.9856003*n)%%360
  ll <- (l + 1.915*sin(g*pi/180) + 0.02*sin(2*g*pi/180))%%360
  ep <- 23.439 - 0.0000004*n
  sin_rad <- cos(ep*pi/180)*sin(ll*pi/180)
  cos_rad <- cos(ll*pi/180)
  sin_deg <- sin(ep*pi/180)*sin(ll*pi/180)
  
  ang_rad <- (atan(sin_rad/cos_rad)*180/pi)%%360
  if (cos_rad < 0){ang_rad <- ang_rad +180}
  if (sin_rad < 0 & cos_rad > 0){ang_rad <- ang_rad + 360}
  
  dec <- asin(sin_deg)*(180/pi)
  
  gmst_deg <- ((6.697375 + 0.0657098242*n + time_utc)%%24)*15
  lmst_deg <- gmst_deg + lon
  
  hour_ang <- lmst_deg - ang_rad
  
  sin_el <- sin(dec*pi/180)*sin(lat*pi/180)  + cos(dec*pi/180)*cos(lat*pi/180)*cos(hour_ang*pi/180)
  el <- asin(sin_el)*180/pi
  
  sin_az <- -cos(dec*pi/180)*sin(hour_ang*pi/180)/cos(el*pi/180)
  cos_az <- (sin(dec*pi/180) - (sin(el*pi/180)*sin(lat*pi/180)))/(cos(el*pi/180)*cos(lat*pi/180))
  az <- atan(sin_az/cos_az)*180/pi
  
  if (cos_az < 0){az <- az +180}
  if (sin_az < 0 & cos_az > 0){az <- az + 360}
  
  return(list("sun_el"=el, "sun_az"=az))
  
}

# Atmospheric refraction model:
# From K model refraction formulae (Holleman & Huuskonen, 2013):
ref_kmodel <- function(el, k, n0, hkmasl, radius_earth){
  a <- n0*1e-6
  b <- ((k-1)/(2*k-1))*cos(el*pi/180)
  c <- (sin(el*pi/180)^2) + 2*((2*k-1)/(k-1))*a + 2*((2*k-1)/(k*radius_earth))*hkmasl
  ref_ang <- b*(sqrt(c) - sin(el*pi/180))*180/pi # refraction angle in degrees
  return(ref_ang)
}

#########################################################################################
## DEFAULT SETTINGS #####################################################################

# Date, radar abbrv. and input .tgz file if not provided as command line arguments:
date_str <- "170720"
radar <- "CDV"

work_path <- "/home/operator/progR/SunINTFCal"
ini_file <- paste(work_path, "SunINTFCal.ini", sep="/")

# Default initialization settings (in case default settings file is not found)
scan_speed <- 24 # Antenna scan speed [deg/s]
k <- 5/4 # k-model constant
n0 <- 313 # Surface refractivity
radius_earth <- 6378 # Earth radius [km]
bin_frac_th <- 0.9 # Min. valid bin fraction
range1_cl <- 50 # Range threshold for valid bin fraction calculation [km]
range2_pow <- 80 # Range threshold for median power calculation [km]
pow_sd_max <- 2 # Max. allowed SD from median power [dB]


#########################################################################################
# SETTINGS #############################################################################

# Interference detection and characterization uses non-corrected reflectivity (T)
data_sel <- 1 # Select data-type (1=DBT, 2=DBZ, 3=VEL)

# List of available data types in RAW product (currently only 1-3 available):
data_type_l <- list("0"= "XHDR", "1"=  "DBT", "2"= "DBZ", "3"= "VEL", "4"= "WIDTH", "5"= "ZDR")

# Receiver band width (while not correct in the headers)
rec_bw_l <- list("CDV"=232, "LMI"=232, "PBE"=227, "PDA"=227)

#########################################################################################
## ARGUMENTS ############################################################################

#Retrieve command-line argument(s): date and radar name
options <- commandArgs(trailingOnly=T)

if (!is.na(options[1])){
  date_str <- options[1]
}
date_obj <- as.Date(date_str, format="%y%m%d")

if (!is.na(options[2])){
  radar <- options[2]
}

if (!is.na(options[3])){
  tgz <- options[3]
}else{
  date_tgz <- format(as.Date(date_str, format="%y%m%d"), format="%Y%m%d")
  tgz <- paste(radar, "RAW", date_tgz, ".tgz", seP="")
}

if (!is.na(options[4])){
  work_path <- options[4]
}

# LOG file:
log_path <- paste(work_path, radar, "LOG/", sep="/")
out_file_log <- paste(log_path, "LOGsunINTF_", date_str, "_", radar, ".txt", sep="")
if (!file.exists(log_path)){dir.create(log_path, showWarnings =F, recursive = T)}

cat("\n# DATE: ", date_str, "\n# RADAR: ", radar, "\n\n", file=out_file_log, sep="", append=T)
cat("# EXECUTION: a_INTF_detector.R\n  ", as.character(time_now), "\n", sep="",
    file=out_file_log, append=T)
cat("# EXECUTION: a_INTF_detector.R ", as.character(time_now), "\n", sep="")

if (!is.na(options[5])){
  
  ini_file <- options[5]
  cat("  # Settings from: ", ini_file, "\n", file=out_file_log, sep="", append=T)
  
}else{
  # Error message to LOG file:
  cat("  # No settings file specified, default values will be used\n", file=out_file_log, sep="", append=T)
}

if (file.exists(ini_file)) {
  
  # Open connection
  ini_conn <- file(ini_file, "r")
  ini_params <- readLines(ini_conn)
  
  scan_speed <- read_num_hdr(ini_params[grep("scan speed", ini_params)])
  k <- read_num_hdr(ini_params[grep("k-model", ini_params)])
  n0 <- read_num_hdr(ini_params[grep("Surface refractivity", ini_params)])
  radius_earth <- read_num_hdr(ini_params[grep("Earth radius", ini_params)])
  bin_frac_th <- read_num_hdr(ini_params[grep("fraction of valid bins", ini_params)])
  range1_cl <- read_num_hdr(ini_params[grep("Range threshold - bin fraction", ini_params)])
  range2_pow <- read_num_hdr(ini_params[grep("Range threshold - median power", ini_params)])
  pow_sd_max <- read_num_hdr(ini_params[grep("Max. Std. Dev.", ini_params)])
  
  close(ini_conn)
  rm(ini_conn)  
  
}else{
  # Error message to LOG file:
  cat("  # Settings file not found, default values will be used\n", file=out_file_log, sep="", append=T)
}

#########################################################################################
## VARIABLES ############################################################################

# Define outpath based on directory tree structure and build it if not already:
out_path <- paste(work_path, radar, "Rec/", sep="/")
if (!file.exists(out_path)){dir.create(out_path, showWarnings =F, recursive = T)}
# Output file where information of interferences is to be recorded
out_file <- paste(out_path, "INTF_", date_str, "_", radar, ".txt", sep="")
cat("  # Output File: ", out_file, "\n\n", file=out_file_log, sep="", append=T)

# Input folder:
in_path <- paste(work_path, "tmp/", sep="/")

# # Raw file selection from input folder (only short range task):
# command <- paste("grep -L \"PPIVOL_A\" ", in_path, radar, date_str, "*.RAW*", sep="")

# Raw file selection from input folder (both short and long range tasks):
command <- paste("grep -L ", in_path, radar, date_str, "*.RAW*", sep="")
filenames <- system(command,intern=T)

#########################################################################################
#####  SCRIPT CORE  #####################################################################
#########################################################################################

DF_tmp <- list()
counter_file <- 0

# Output file is only generated if there's at least one RAW file
if (length(filenames)!=0){
  

  # Loop through selected .RAW files:
  for (f in filenames){
    
    # Counters to be used in output to LOG file:
    counter_intf <- 0 # counter for number of interferences detected
    counter_file <- counter_file + 1 # counter for parsed RAW file
    cat("  # Input file no.", counter_file,": ", gsub(in_path,"",f), 
        file=out_file_log, sep="", append=T)
    
    # Memory-mapping of file and separation into header and volume data:
    file_map <- mmap(file = f, mode = raw())
    hdr <- file_map[1:(2*6144)] # headers
    m <- file_map[(2*6144 + 1): length(file_map)] # ray data
    rm(file_map)
    
    #####  RAW HEADER INFORMATION  ##########################################################
    
    # Volume and product date and time (to derive the time of the interferences)
    vol_time <- time(hdr,6245)
    prod_time <- time(hdr,33)
    time_i <- vol_time$f_time
    vol_date <- vol_time$f_date
    vol_date <- as.character(as.Date(vol_date, format="%Y/%m/%d"))
    
    ray_hdr_size <- readBin(hdr[6269:6270], integer(), size=2, n=1, signed=T)
    
    radar_wl <- readBin(hdr[481:484], integer(), size=4, n=1, signed=T)/100
    
    # Radar latitude and longitude (to derive sun position at the time of the interference)
    radar_lat <- readBin(hdr[6325:6328], integer(), size=4, n=1, signed=T)*360/(2^32)
    radar_lon <- readBin(hdr[6329:6332], integer(), size=4, n=1, signed=T)*360/(2^32)
    
    radar_hmasl <- readBin(hdr[6345:6348], integer(), size=4, n=1, signed=T)/100
    
    # Data types contained in the RAW file
    mask0 <- readBin(hdr[6773:6776], integer(), size=4, n=1, signed=F)
    mask0_desc <- read_bits(mask0, 32)
    data_types <- lapply(mask0_desc, function(x){return(data_type_l[[x]])})
    
    a <- readBin(hdr[6939:6940], integer(), size=2, n=1, signed=F)/100000
    
    # Calibration constants
    radar_zcal <- (readBin(hdr[7107:7108], integer(), size=2, n=1, signed=T))/16
    radar_cnt <- (readBin(hdr[7145:7146], integer(), size=2, n=1, signed=T))/100
    
    # Receiver bandwidth (uncomment when header value is correct)
    #   rec_bw <- (readBin(hdr[7149:7151], integer(), size=2, n=1, signed=F))
    rec_bw <- rec_bw_l[[radar]]
    
    # Range of area scanned, range bin number and bin size:
    range_start <- (readBin(hdr[7409:7412], integer(), size=4, n=1, signed=T))/100
    range_end <- (readBin(hdr[7413:7416], integer(), size=4, n=1, signed=T))/100
    bins <- readBin(hdr[7419:7420], integer(), size=2, n=1, signed=T)
    bin_step <- readBin(hdr[7425:7428], integer(), size=4, n=1, signed=T)/100
    
    radial_res <- readBin(hdr[7571:7572], integer(), size=2, n=1, signed=T)/1000
    
    # Number of sweeps (elevations) performed:
    sweeps <- readBin(hdr[7575:7576], integer(), size=2, n=1, signed=T)
    
    # Beamwidths in azimuthal and zenithal directions:
    bw_h <- readBin(hdr[7953:7956], integer(), size=4, n=1, signed=F)*360/(2^32)
    bw_v <- readBin(hdr[7957:7960], integer(), size=4, n=1, signed=F)*360/(2^32)
    
    rm(hdr)
    
    data_bytes <- length(m)
    record_num <- data_bytes/6144
    
    if ((data_bytes%%6144)!=0){
      cat("    Possible problem in RAW file: file length and record number do not match\n", file=out_file_log, sep="", append=T)
    }
    
    ##### WRITE OUTPUT HEADER ###########################################################
    
    if (!file.exists(out_file)) {
      
      H1 <- "# CONSTANT INTERFERENCES"
      H2 <- paste("# File created:", time_now, sep=" ")
      H3 <- paste("# Input file:", tgz, sep=" ")
      H4 <- paste("# Initialization file:", ini_file, sep=" ")
      H5 <- paste("# Radar abbrv.:", radar, sep=" ")
      H6 <- paste("# Radar height:", radar_hmasl, "masl", sep=" ")
      H7 <- paste("# Radar wavelength:", radar_wl, "cm", sep=" ")
      H8 <- paste("# Radar cnt:", radar_cnt, "dBZ", sep=" ")
      H9 <- paste("# Calib. cnt:", radar_zcal, "dBm", sep=" ")
      H10 <- paste("# Receiver bandwidth:", rec_bw, "kHz", sep=" ")
      H11 <- paste("# Beam width H:", round(bw_h,2), "deg", sep=" ")
      H12 <- paste("# Beam width V:", round(bw_v,2), "deg", sep=" ")
      H13 <- paste("# Radial resolution (nominal):", radial_res, "deg", sep=" ")
      
      header <- c(H1, H2, H3, H4, H5, H6, H7, H8, H9,  H10, H11, H12, H13, "#", "#")
      write(header, file=out_file, ncolumns=15, sep="\n", append=T)
      
      col_names <- c("date", "time", "intf_time", "d_intf", "az_r",
                     "el_r", "az_s", "el_s", "pow", "sd_pow", "bin_frac", "label")
      
      write(col_names, file=out_file, append=T, ncolumns=12)
      
    }
    
    ##### PARAMETER CALCULATION #############################################################
    
    # Create vectors with start, average and end range values for each range bin:
    range_bins <- seq(range2_pow + bin_step/2000, (range_start/1000) + bins*(bin_step/1000)
                      - (bin_step/2000), bin_step/1000)
    
    #####  DATA READING  #########################################
    
    # Variable initialization:
    counter_sw_prev <- 1
    counter_corrupt_ray <- 0
    sw_data <- raw() # In this vector all data for a sweep will be hold in binary format
    data_ray <- c() # This vector will hold decompressed single ray-data
    elevs <- c()
    ratios_all <- c()
    ratios_th <- c()
    
    # Loop for all records, checking which of them pertain to same sweep
    for (i in seq(0, record_num)){
      
      if (i==record_num){counter_sw <- sweeps + 1}else{
        counter_sw <- readBin(c(m[6144*i + 3], m[6144*i + 4]), integer(), 
                              n=1, size=2, signed=T)}
      
      # If sweep number hasn't changed continue loading data
      if (counter_sw == counter_sw_prev){
        
        # Load sweep data into vector but skipping structure headers
        # This avoids the need of distinguishing headers from data while decompressing
        sw_data <- append(sw_data, m[(6144*i + 13):(6144*(i+1))])
        
        # If sweep number changes apply decompression code to already loaded data
      }else{
        
        # counting bytes to access appropriate vector indexes
        # Start skipping sweep headers and go directly into data reading
        bc <- 76*length(data_types) + 1
        j <- 1 # ray counter
        
        # Read 1st ray compression code
        ray_compr_code <- readBin(c(sw_data[bc], sw_data[bc + 1]), integer(),
                                  n=1, size=2, signed=T)
        
        bc <- bc + 2
        
        # While to check end of sweep:
        while(ray_compr_code != 0){
          
          # While to check end of ray:
          while (ray_compr_code != 1){
            
            # Data compression code interpretation:
            # Data reading; all data including ray-header data are read as uint8
            if (ray_compr_code < 0){
              data_ray <- append(data_ray, readBin(
                sw_data[bc:(bc + (2*(32768 + ray_compr_code) - 1))], 
                integer(), n=2*(32768 + ray_compr_code), size=1, signed=F))
              bc <- bc + 2*(32768 + ray_compr_code)
              
            }else if (ray_compr_code > 0){
              data_ray <- append(data_ray, rep.int(NA, 2*ray_compr_code))
            }
            
            # Read next compression code 
            # if =1 end of ray, else continue loading data for current data-ray
            ray_compr_code <- readBin(c(sw_data[bc], sw_data[bc + 1]), 
                                      integer(), n=1, size=2, signed=T)
            bc <- bc + 2
            
          }
          
          # After all data for current data-ray has been loaded, check ray length 
          # and add zeros if shorter than nominal length
          if(length(data_ray) < bins+ray_hdr_size ){
            data_ray <- append(data_ray, 
                               rep.int(NA, bins+ray_hdr_size - length(data_ray)))
            counter_corrupt_ray <- counter_corrupt_ray + 1
          }
          
          # Separate ray header and ray data:
          ray_hdr <- data_ray[1:12]
          data_ray <- data_ray[13:length(data_ray)]
          
          # Substitute non available data (-32dBZ and -31.5dBZ) for NA:
          data_ray[which(data_ray==0)] <- NA
          data_ray[which(data_ray==1)] <- NA
          
          # Select data and corresponding ranges only for bins located further than the 
          # threshold ranges: 
          # 50Km: default to avoid ground clutter from lateral lobes - valid bin fraction - 
          # 80Km: selected to assure no rain signal present (elevations > 3º) - median power along range -
          data_ray1 <- data_ray[ceiling((range1_cl - range_start/1000)/(bin_step/1000))
                                :length(data_ray)]
          data_ray2 <- data_ray
          data_ray2 <- data_ray2[ceiling((range2_pow - range_start/1000)/(bin_step/1000))
                                 :length(data_ray2)]
          
          # Compute the fraction of bins with available data in each threshold-range:
          bin_ratio <- length(which(!is.na(data_ray1)))/(length(data_ray1))
          
          # Focus only in a particular data type; e.g. DBT if data_sel=1          
          data_type_check <- (j-data_sel)%%length(data_types)
          
          ##### INTERFERENCE DETECTION ##################################################
          
          # Selection of continuous interferences (apply the least severe criterium):
          if (data_type_check==0 & bin_ratio>=bin_frac_th){
            
            counter_intf <- counter_intf + 1
            
            # Ray info (start and end azimuth and elevation) from ray-header:
            az_i <- (ray_hdr[2]*2^8 + ray_hdr[1])*360/2^16
            intf_el <- (ray_hdr[4]*2^8 + ray_hdr[3])*360/2^16
            az_f <- (ray_hdr[6]*2^8 + ray_hdr[5])*360/2^16
            
            # Convert ray info into average azimuth and azimuthal scan width:
            if (az_f >= az_i ){
              intf_az <- (1/2*(az_i + az_f))%%360
              intf_width <- az_f - az_i
            } else {
              intf_az <- (1/2*(az_i + (az_f+360)))%%360
              intf_width <- (360 + az_f) - az_i
            }
            
            # Compute time of interference from volume start time, elevation number, 
            # ray position in the scan and antenna scan speed:  
            vol_time_dec <- as.integer(substr(time_i,1,2)) + 
              as.integer(substr(time_i,4,5))/60 + 
              as.integer(substr(time_i,7,8))/3600
            intf_time <- vol_time_dec + (((counter_sw_prev - 1)*360 + intf_az)/scan_speed)/3600
            
            # Calculate median power for each of the threshold-range data:
            dbm <- 1/2*(data_ray2-64) - radar_cnt - 20*log10(range_bins) - a*range_bins
            dbm_avg <- median(dbm, na.rm=T)
            dbm_sd <- (1/0.6745)*median(abs(dbm-dbm_avg), na.rm=T)
            
            # Calculate Solar position (adding refraction in the apparent elevation calc.):
            sun_coords <- sun_pos_fn(radar_lat, radar_lon, date_obj, intf_time)
            sun_coords$sun_el <- sun_coords$sun_el + ref_kmodel(sun_coords$sun_el, k, n0, radar_hmasl/1000, radius_earth)
            
            # Load all relevant information about interference into vector and write in 
            # output file:
            if (dbm_sd<=pow_sd_max){
              cnt_lab <- "constant"
            }else{
              cnt_lab <- "non-constant"
            }
            
            INTF_data <- c(vol_date, sprintf("%.4f", round(c(vol_time_dec, intf_time),4)),
                           sprintf("%.2f", round(c(intf_width, intf_az, intf_el, sun_coords$sun_az,
                                                   sun_coords$sun_el, dbm_avg, dbm_sd, bin_ratio),2)), cnt_lab)
            write(INTF_data, file=out_file, append=T, ncolumns=12)
            
          }
          
          j <- j + 1
          data_ray <- c()
          data_ray1 <- c()
          data_ray2 <- c()
          
          # Read next compression code 
          # if =0 end of sweep, else continue on with new data-ray
          ray_compr_code <- readBin(c(sw_data[bc], sw_data[bc + 1]), 
                                    integer(), n=1, size=2, signed=T)
          
          bc <- bc + 2
          
        }
        sw_data <- raw()
        if (i!=record_num){ sw_data <- m[(6144*i + 13):(6144*(i+1))]}
        
      }
      
      counter_sw_prev <- counter_sw
      
    }
    
    cat("  Det. interfs.: ", counter_intf, "\n", file=out_file_log, sep="", append=T)
    if ((counter_sw_prev-1)!=sweeps){
      cat("    Possible problem in RAW file: Number of sweeps does not match nominal\n", 
          file=out_file_log, sep="", append=T)
    }
    # warning if radial rays are not complete (5400 is the number of non-complete rays by default in VOLB tasks)
    if (radar=="CDV"){
      SemiRays <- 6480
    }else{
      SemiRays <- 5400
    }
    if (counter_corrupt_ray!=0 & counter_corrupt_ray>SemiRays){
      cat("    Possible problem in RAW file: ", (counter_corrupt_ray-SemiRays), " non-complete rays found\n",
          sep="", file=out_file_log, append=T)
    }
    
  }
  
}else{
  # Error message to LOG file:
  cat("  # NO RAW INPUT FILES FOUND\n", file=out_file_log, sep="", append=T)
  cat("    NO OUTPUT FILE GENERATED\n", sep="", file=out_file_log, append=T)
  
}

#########################################################################################
## EOS ##
#########################################################################################

