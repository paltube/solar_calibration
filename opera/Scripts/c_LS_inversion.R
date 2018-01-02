#!/usr/bin/env Rscript

#########################################################################################

# Program in R - Inversions using solar observations and scatter plots
# P. Altube Vazquez - July 2017

##### INFO and USAGE ####################################################################

# This script has been developed for its automatic and daily execution within the solar 
# monitoring project (SunINTFCal). The shell script that boots up the daily proccess is: 
# "SunINTFCal_daily.sh"
#                                                                         
# This script performs model inversions (linear LS retrievals) using  daily solar  
# interferences detected (input file: "SunINTF_YYMMDD_XXX.txt"). Daily reference solar 
# flux data is retrieved from the DRAO observatory website. Solar flux data is used in 
# comparison with peak power retrieved in the LS fit for monitoring the calibration of 
# the receiver chain.        
#                                                                                       
# Resulting model parameters and their uncertainties are daily recorded in monthly files
# for each particular radar: "Calib_5P_1day_YYMM_XXX.txt", "Calib_3P_1day_YYMM_XXX.txt",
# "Calib_5P_3day_YYMM_XXX.txt" and "Calib_3P_3day_YYMM_XXX.txt". A scatter plot of the 
# solar observations and model fit isolines is also output for each inversion case: 
# "ScatterFit_5P_1day_PDA.png", "ScatterFit_3P_1day_PDA.png", "ScatterFit_5P_3day_PDA.png"
# and "ScatterFit_3P_3day_PDA.png".
#                                                                                       
# Command line execution: specify date and radar abbrv., work path and settings file.   
# Rscript /.../Scripts/c_LS_inversion.R YYMMDD XXX /... settings.ini                          

#########################################################################################

options(warn=-1)

library(getopt, quietly=T)
library(XML, quietly=T)
library(RCurl, quietly=T)
library(stats, quietly=T)
library(plotrix, quietly=T)

time_now <- Sys.time()

#########################################################################################
##  FUNCTIONS  ##########################################################################

# Load reference data from file or url
load_drao <- function(date, url_h, url_d, file_db, write_file=TRUE){
  
  miss_file <- TRUE
  nodata_file <- TRUE
  read_url_h <- TRUE
  miss_url_h <- TRUE
  err_url_h <- TRUE
  read_url_d <- TRUE
  miss_url_d <- TRUE
  err_url_d <- TRUE
  miss_ref <- TRUE
  ref <- NA
  sf <- NA
  
  err_msg <- "  # "
  
  date <-  as.Date(date,format="%Y-%m-%d")
  
  if (file.exists(file_db)){
    
    miss_file <- FALSE
    file_data <- read.table(file=file_db, header=T)
    file_data$date <- as.Date(as.character(file_data$date), format="%Y-%m-%d")
    matches <- file_data[file_data$date==date,]
    
    if (nrow(matches)!=0){
      
      nodata_file <- FALSE
      miss_ref <- FALSE
      ref <- file_db
      
      h_matches <- matches[matches$type=="H",]
      d_matches <- matches[matches$type=="D",]
      
      if (nrow(h_matches)!=0){
        
        read_url_h <- FALSE
        read_url_d <- FALSE
        miss_url_h <- FALSE
        err_url_h <- FALSE
        miss_url_d <- FALSE
        err_url_d <- FALSE
        
        write_file <- FALSE
        
        sf <- median(h_matches$sf, na.rm=T)
        
      }else{
        
        sf <- median(d_matches, na.rm=T)
        
      }
      
    }
    
  }
  
  if (read_url_h){
    
    if (url.exists(url_h)){
      
      miss_url_h <- FALSE
      url_h_table <- readHTMLTable(url_h)
      
      if (!is.null(url_h_table[[1]])){
        
        err_url_h <- FALSE
        drao_data <- data.frame(url_h_table[[1]])
        drao_data$Date <- as.Date(drao_data$Date,format="%Y-%m-%d")
        
        matches <- as.numeric(as.character(drao_data[drao_data$Date==date,"Observed.Flux"]))
        
        if (length(matches)!=0){
          
          read_url_d <- FALSE
          miss_url_d <- FALSE
          err_url_d <- FALSE
          miss_ref <- FALSE
          ref <- url_h
          
          sf <- median(matches, na.rm=T)
          type <- "H"
          time <- "median"
          
        }else{
          
          if (!nodata_file){
            ref <- file_db
          }
          
        }
        
      }
      
    }
    
  }
  
  if (read_url_d){
    
    if (url.exists(url_d)){
      
      miss_url_d <- FALSE
      url_d_table <- readHTMLTable(url_d)
      
      if (!is.null(url_d_table[[1]])){
        
        err_url_d <- FALSE
        drao_data <- data.frame(url_d_table)
        col <- colnames(drao_data)[1]
        drao_date <- substr(x=gsub(".", "", gsub(pattern="[A-z]",replacement="", col), fixed=T),
                            start=1, stop=8)
        drao_time <- substr(x=gsub(".", "", gsub(pattern="[A-z]",replacement="", col), fixed=T),
                            start=9, stop=12)
        drao_date <- as.Date(x=drao_date, format="%Y%m%d")
        colnames(drao_data) <- c("Quantity", "Value")
        drao_data$Quantity <- as.character(drao_data$Quantity)
        drao_data$Value <- as.numeric(as.character(drao_data$Value))
        
        if (drao_date==date){
          
          sf <- as.numeric(DRAOdata[DRAOdata$Quantity=="Observed Flux Density", "Value"])
          time <- drao_time
          type <- "D"
          
          miss_ref <- FALSE
          ref <- url_d
          
        }
        
      }
      
    }
    
  }
  
  if (write_file & !miss_ref){
    
    df_out <- data.frame(date=date, time=time, sf=sprintf("%.1f", round(sf,1)), type=type)
    
    if (miss_file){
      
      write.table(x=df_out, file=file_db, append=F, quote=F, row.names=F, col.names=T)
      
    }else{
      
      write.table(x=df_out, file=file_db, append=T, quote=F, row.names=F, col.names=F)
      
    }   
    
  }
  
  if (miss_url_h){
    err_msg <- "  # Failed access to DRAO web page (historic database)"
  } else if (err_url_h){
    err_msg <- "  # DRAO web page (historic database) seems to have changed"
  }
  
  
  if(miss_ref){
    
    if (miss_url_d){
      err_msg <- c(err_msg, "  # Failed access to DRAO web page (daily flux observation)")
    } else if (err_url_d){
      err_msg <- c(err_msg, "  # DRAO web page (daily flux observation)) seems to have changed")
    }
    
    log_msg <- paste("# ", date, " NO reference flux data available ", ref, sep="")
    
  }else{
    log_msg <- paste("# ", date, " reference flux from: ", ref, sep="")
  }
  
  res <-  data.frame(sf=sf, log_msg=log_msg, err_msg=err_msg)
  return(res)
  
}

# Convert 10.7cm flux to 5cm flux 
solar_flux_5cm <- function(Fdrao){
  
  SFU5 <- 0.71*(Fdrao - 64) + 126 # solar flux at C band [sfu] [Tapping, 2001]
  return(SFU5)
  
}

# Conversion from SFU to mW using radar parameters
sfu_to_mw <- function(rBW, WL, gain_ant){
  A <- (((WL/100)^2)*gain_ant)/(4*pi) # effective area of antenna [m²]
  SFU2mWfac <- (1/2)*1e-13*(rBW/1000)*A
  return(SFU2mWfac)
}

# Extract numerical values from header
read_num_header <- function(hdrline){
  NUM <- gsub("^(([^:]+):)","", hdrline)
  NUM <- gsub("[A-Z]","", NUM)
  NUM <- gsub("[a-z]","", NUM)
  NUM <- as.numeric(NUM)
  return(NUM)
}

erf <- function(x){
  y <- 2*pnorm(sqrt(2)*x)-1
  return(y)
}

# Trascendental equation for estimation of azimuthal width
rootf <- function(x,a,b){
  y <- erf(a*x+b)-erf(a*x-b)-(2/exp(1))*erf(b)
  return(y)
}

solar_widths_scan <- function(d_conv_h, d_conv_v, d_radial){
  
  azs <- seq(-100,100, 0.01)
  a <- sqrt(4*log(2))/d_conv_h
  b <- a*(d_radial/2)
  root <- uniroot(f = rootf, interval = c(0,(1/2)*max(azs)), a=a, b=b)
  dx_eff <- sqrt(4*log(2))*(root$root)
  dx_eff <- round(dx_eff, 2)
  dy_eff <- round(d_conv_v, 2)
  
  return(list("h"=dx_eff, "v"=dy_eff))
  
}

l_scan_calc <- function(d_radial, d_sun, d_beam, d_conv){
  
  l0 <- (1/log(2))*(d_beam/d_sun)^2*(1-exp(-log(2)*(d_sun/d_beam)^2))
  D <- 4*log(2)
  rat <- (d_conv/d_radial)
  
  lscan <- l0*sqrt((pi/D))*rat*erf(sqrt(log(2))*(1/rat))
  
  Lsc <- 10*log10(lscan)
  return(Lsc)
  
}

# Model inversion
sun_fit_3P <- function(x, y, z, dx, dy, d_radial){
  
  x0 <- NA
  y0 <- NA
  p0 <- NA
  rmse <- NA
  r_sq <- NA
  sd_Dx <- NA
  sd_Dy <- NA
  sd_x0 <- NA
  sd_y0 <- NA
  sd_p0 <- NA
  a1 <- NA
  a2 <- NA
  b1 <- NA
  b2 <- NA
  c <- NA
  
  suf_pars <- FALSE
  all_pars <- FALSE
  fis_pars <- FALSE
  
  # Calculate effective width in azimuthal direction
  dx_eff <- solar_widths_scan(dx, dy, d_radial)$h
  dx_eff <- round(dx_eff, 2)
  dy_eff <- solar_widths_scan(dx, dy, d_radial)$v
  a1 <- -40*log10(2)*(1/(dx_eff^2))
  a2 <- -40*log10(2)*(1/(dy_eff^2))
  
  N <- length(z)
  
  z <- z - a1*(x^2) - a2*(y^2)
  
  if (N > 3){
    
    suf_pars <- TRUE
    
    fit <- lm(z ~ x + y + 1)
    fit_stat <- summary(fit)
    
    if (dim(coef(fit_stat))[1]==3){
      
      all_pars <- TRUE
      
      # Retrieve parameters and their errors from inversion results
      c <- fit$coefficients[1]
      sd_c <- coef(fit_stat)[1, "Std. Error"]
      b1 <- fit$coefficients[2]
      sd_b1 <- coef(fit_stat)[2, "Std. Error"]
      b2 <- fit$coefficients[3]
      sd_b2 <- coef(fit_stat)[3, "Std. Error"]
      
      if (a1<0 & a2<0){
        
        fis_pars <- TRUE
        # Compute parameters of interest from retrieved parameters
        x0 <- -b1/(2*a1)
        y0 <- -b2/(2*a2)
        p0 <- c - (b1^2)/(4*a1) - (b2^2)/(4*a2)
        
        # Goodness of fit
        r_sq <- fit_stat$adj.r.squared
        rmse <- sqrt((1/(length(fit_stat$residuals)-3-1))*sum((fit_stat$residuals)^2))
        
        # Compute their errors using error propagation             
        sd_x0 <- abs(b1/(2*a1))*abs(sd_b1/b1)
        sd_y0 <- abs(b1/(2*a2))*abs(sd_b2/b2)
        e1 <- (1/2)*abs((b1/a1)*sd_b1)
        e2 <- (1/2)*abs((b2/a2)*sd_b2)
        sd_p0 <- sqrt((sd_c^2) + e1^2 + e2^2)
        
        # Format results for registration
        y0 <- round(y0,2)
        x0 <- round(x0,2)
        p0 <- round(p0,2)
        rmse <- round(rmse, 2)
        r_sq <- round(r_sq,2)
        sd_x0 <- round(sd_x0,2)
        sd_y0 <- round(sd_y0,2)
        sd_p0 <- round(sd_p0,2)
        
        log_msg <- "Inversion OK"
        warn_msg <- ""
        
      }
      
    }
    
  }
  
  if (!suf_pars){
    log_msg <- "Not enough solar interferences available for inversion"
    warn_msg <- "Not enough interferences"
  }
  if (!all_pars){
    log_msg <- "Inversion has not defined all model parameters"
    warn_msg <- "Model not well defined"
  }
  
  if (!fis_pars){
    log_msg <- "Inversion yields non-physical parameters"
    warn_msg <- "Non-physical solution"
  }
  
  a1 <- round(a1, 2)
  a2 <- round(a2, 2)
  b1 <- round(b1, 2)
  b2 <- round(b2, 2)
  c <- round(c, 2)
  
  res <- data.frame(a1=a1, a2=a2, b1=b1, b2=b2, c=c, p0=p0, sd_p0=sd_p0, x0=x0, sd_x0=sd_x0, y0=y0, 
                    sd_y0=sd_y0, dx=dx_eff, sd_Dx= sd_Dx, dy=dy_eff, sd_Dy=sd_Dy, rmse=rmse, r_sq=r_sq,
                    log_msg=log_msg, warn=warn_msg)
  return(res)
  
}

sun_fit_5P <- function(x, y, z, dx, dy, d_radial){
  
  x0 <- NA
  y0 <- NA
  p0 <- NA
  rmse <- NA
  r_sq <- NA
  sd_Dx <- NA
  sd_Dy <- NA
  sd_x0 <- NA
  sd_y0 <- NA
  sd_p0 <- NA
  dx_eff <- NA
  dy_eff <- NA
  a1 <- NA
  a2 <- NA
  b1 <- NA
  b2 <- NA
  c <- NA
  
  suf_pars <- FALSE
  all_pars <- FALSE
  fis_pars <- FALSE
  
  N <- length(z)
  
  if (N > 5){
    
    suf_pars <- TRUE
    
    fit <- lm(z ~ I(x^2) + I(y^2) + x + y + 1)
    fit_stat <- summary(fit)
    
    if (dim(coef(fit_stat))[1]==5){
      
      all_pars <- TRUE
      
      # Retrieve parameters and their errors from inversion results
      c <- fit$coefficients[1]
      sd_c <- coef(fit_stat)[1, "Std. Error"]
      a1 <- fit$coefficients[2]
      sd_a1 <- coef(fit_stat)[2, "Std. Error"]
      a2 <- fit$coefficients[3]
      sd_a2 <- coef(fit_stat)[3, "Std. Error"]
      b1 <- fit$coefficients[4]
      sd_b1 <- coef(fit_stat)[4, "Std. Error"]
      b2 <- fit$coefficients[5]
      sd_b2 <- coef(fit_stat)[5, "Std. Error"]
      
      if (a1<0 & a2<0){
        
        fis_pars <- TRUE
        # Compute parameters of interest from retrieved parameters
        x0 <- -b1/(2*a1)
        y0 <- -b2/(2*a2)
        p0 <- c - (b1^2)/(4*a1) - (b2^2)/(4*a2)
        dx_eff <- sqrt(-(40*log10(2))/a1)
        dy_eff <- sqrt(-(40*log10(2))/a2)
        
        # Goodness of fit
        r_sq <- fit_stat$adj.r.squared
        rmse <- sqrt((1/(length(fit_stat$residuals)-5-1))*sum((fit_stat$residuals)^2))
        
        # Compute their errors using error propagation             
        sd_Dx <- (1/2)*sd_a1*sqrt(-(40*log10(2))/(a1^3))
        sd_Dy <- (1/2)*sd_a2*sqrt(-(40*log10(2))/(a2^3))
        sd_x0 <- abs(b1/(2*a1))*sqrt((sd_b1/b1)^2 + (sd_a1/a1)^2)
        sd_y0 <- abs(b1/(2*a2))*sqrt((sd_b2/b2)^2 + (sd_a2/a2)^2)
        e1 <- (1/2)*abs((b1/a1)*sd_b1)
        e2 <- (1/2)*abs((b2/a2)*sd_b2)
        e3 <- (1/4)*abs(((b1/a1)^2)*sd_a1)
        e4 <- (1/4)*abs(((b2/a2)^2)*sd_a2)
        sd_p0 <- sqrt((sd_c^2) + e1^2 + e2^2 + e3^2 + e4^2)
        
        # Format results for registration
        y0 <- round(y0,2)
        x0 <- round(x0,2)
        p0 <- round(p0,2)
        dx_eff <- round(dx_eff,2)
        dy_eff <- round(dy_eff,2)
        rmse <- round(rmse, 2)
        r_sq <- round(r_sq,2)
        sd_x0 <- round(sd_x0,2)
        sd_y0 <- round(sd_y0,2)
        sd_p0 <- round(sd_p0,2)
        sd_Dx <- round(sd_Dx,2)
        sd_Dy <- round(sd_Dy,2)
        
      }
      
    }
    
  }
  
  if (!suf_pars){
    log_msg <- "Not enough solar interferences available for inversion"
    warn_msg <- "Not enough interferences"
    
  } else if (!all_pars){
    log_msg <- "Inversion has not defined all model parameters"
    warn_msg <- "Model not defined"
    
  } else if (!fis_pars){
    log_msg <- "Inversion yields non-physical parameters"
    warn_msg <- "Non-physical solution"
    
  } else {
    log_msg <- "Inversion OK"
    warn_msg <- ""
  }
  
  a1 <- round(a1, 2)
  a2 <- round(a2, 2)
  b1 <- round(b1, 2)
  b2 <- round(b2, 2)
  c <- round(c, 2)
  
  res <- data.frame(a1=a1, a2=a2, b1=b1, b2=b2, c=c, p0=p0, sd_p0=sd_p0, x0=x0, sd_x0=sd_x0, 
                    y0=y0, sd_y0=sd_y0, dx=dx_eff, sd_Dx= sd_Dx, dy=dy_eff, sd_Dy=sd_Dy, rmse=rmse, 
                    r_sq=r_sq, log_msg=log_msg, warn=warn_msg)
  return(res)
  
}

sun_fit <- list("3P"=sun_fit_3P, "5P"=sun_fit_5P)

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
## DEFAULT SETTINGS ##################################################################

# Date, radar abbrv. and input .tgz file if not provided as command line arguments:
date1_str <- "170808"
radar <- "CDV"

work_path <- "/home/operator/progR/SunINTFCal"
ini_file <- paste(work_path, "SunINTFCal.ini", sep="/")

d_beam_nom_h <- 1.2
d_beam_nom_v <- 1.1

d_conv_nom_h <- 1.25
d_conv_nom_v <- 1.15

d_ray_nom <- 1
d_sun <- 0.57

gain_ant <- 10^(44.3/10) # Antenna gain (nominal) at 5.6GHz [linear units]
sigma_max <- 2 # Max. number of sigma-intervals allowed for outlier removal

# Urls from which to get reference solar flux data
url_drao_daily <- "http://www.spaceweather.gc.ca/solarflux/sx-4-eng.php"
url_drao <- "http://www.spaceweather.gc.ca/data-donnee/sol_flux/sx-5-flux-eng.php?year="

#########################################################################################
##  SETTINGS  ###########################################################################

# Number of header lines in the input file:
hdr_length <- 15

zcal_ref <- -40

# Colorbar breaks (discrete)
brks_cnt <- seq(-117, -107)
brks_int <- c(-Inf, seq(-116, -106), Inf)

# Colorbar labels
labs <- c(paste("<", as.character(brks_int[-1])))

# Extended colorbar
cols_names <- c("grey75", colorRampPalette(c("blue", "yellow", "red"))(length(brks_int)-3), "black")

cols2_names <- colorRampPalette(c("blue", "yellow", "red"))(length(brks_cnt)-3)

# # Reduced colorbar (without upper and lower bound colors)
cols2_names <- rev(c( "midnightblue", "blue3", "cornflowerblue" ,
                      "darkorchid4", "magenta3", "red3", "orangered",
                      "orange"))

# Include transparency (alpha)
mat_cols <- col2rgb(cols_names)/256
cols <- rgb(mat_cols[1,], mat_cols[2,], mat_cols[3,], alpha=0.7)

mat_cols2 <- col2rgb(cols2_names)/256
cols2 <- rgb(mat_cols2[1,], mat_cols2[2,], mat_cols2[3,], alpha=1)

# Plot limits
lims <- c(-1.1, 1.1)
# Minor breaks
minor <- seq(lims[1], lims[2], 0.1)
mat_col_minor <- col2rgb("grey80")/256
col_minor <- rgb(mat_col_minor[1,], mat_col_minor[2,], mat_col_minor[3,], alpha=0.6)

#########################################################################################
## PARAMETERS ###########################################################################

xf <- seq(-5,5,0.01)
yf <- seq(-5,5,0.01)

types <- c("1day", "3day")
invs <- c("3P", "5P")

#########################################################################################
## ARGUMENTS ############################################################################

#Retrieve command-line argument(s): Date and radar name
options <- commandArgs(trailingOnly=T)

if (length(options)==4){
  
  if (!is.na(options[1])){
    date1_str <- options[1]
  }
  if (!is.na(options[2])){
    radar <- options[2]
  }
  if (!is.na(options[3])){
    work_path <- options[3]
  }
  if (!is.na(options[4])){
    
    ini_file <- options[4]
    
  }else{
    
    set_err <- paste("  # No settings file specified, default file will be used\n",sep="")
  }
  
}else{
  set_err <- paste("  # Error in command line arguments, default settings will be used\n", sep="")  
}

# LOG file:
log_path <- paste(work_path, radar, "LOG/", sep="/")
out_file_log <- paste(log_path, "LOGsunINTF_", date1_str, "_", radar, ".txt", sep="")

if (!file.exists(log_path)){dir.create(log_path, showWarnings =F, recursive = T)}
cat("\n# EXECUTION: c_LS_inversion.R\n  ", as.character(time_now), 
    "\n", sep="", file=out_file_log, append=T)
cat("\n# EXECUTION: c_LS_inversion.R ", as.character(time_now), "\n", sep="")

if (exists("set_err")){
  cat(set_err, file=out_file_log, append=T)
}
cat("  # Settings file: ", ini_file, "\n  ", file=out_file_log, append=T)

# Load initialisation variables from file
if (file.exists(ini_file)) {
  
  # Open connection
  ini_conn <- file(ini_file, "r")
  ini_params <- readLines(ini_conn)
  
  gain_ant <- 10^(read_num_header(ini_params[grep("Antenna gain", ini_params)])/10)
  sigma_max <- read_num_header(ini_params[grep("sigma-intervals", ini_params)])
  
  close(ini_conn)
  rm(ini_conn)  
  
}else{
  cat("    Initialization file not found, default values will be used\n", file=out_file_log, append=T)
}

#########################################################################################
## VARIABLES ############################################################################

# Formatting dates
date1_obj <- as.Date(date1_str, format="%y%m%d")
date2_obj <- date1_obj-1
date3_obj <- date1_obj-2
date2_str <- format(date2_obj, format="%y%m%d")
date3_str <- format(date3_obj, format="%y%m%d")

date_str_seq <- c(date1_str, date2_str, date3_str)
date_obj_seq <- c(date1_obj, date2_obj, date3_obj)

year1_long <- format(date1_obj, format="%Y")
year1 <- format(date1_obj, format="%y")
month1 <- format(date1_obj, format="%m")

# URL from which to retrieve the reference solar flux at the TOA (DRAO observatory)
# Historic record of measured values (ca. 2-3 measurements per day)
url_drao_hist <- paste(url_drao, year1_long, sep="")
# DRAO data file
file_drao <- paste(work_path, "DRAOhistoric.txt", sep="/")

# Output paths
out_path <- paste(work_path, radar, sep="/")
# OUTpath <- paste(work_path, radar, year1_long, month1, sep="/")
plot_path <- paste(out_path, "Plots/", sep="/")
if (!file.exists(plot_path)){dir.create(plot_path, showWarnings =F, recursive = T)}

# Input files:
in_files <- c()
for (d_str_i in seq(1, length(date_str_seq))){
  d_str <- date_str_seq[d_str_i]
  d_obj <- date_obj_seq[d_str_i]
  YY <- format(d_obj, format="%Y")
  MM <- format(d_obj, format="%m")
  in_files <- c(in_files, paste(paste(out_path, "Rec/", sep="/"),"SunINTF_", d_str, "_", radar, ".txt", sep=""))
}

## REFERENCE SOLAR FLUX ##
# Load reference from file or url, for each of the three dates

SF <- data.frame(row.names = sort(date_obj_seq))
SF$SFU10 <- rep(NA, length(date_obj_seq))

for (d_str_j in seq(1, length(date_str_seq))){
  
  d_str <- rownames(SF)[d_str_j]
  d_obj <- as.Date(d_str)
  
  SF10_tmp <- load_drao(d_obj, url_drao_hist, url_drao_daily, file_drao)
  SF[d_str, ] <- SF10_tmp$sf
  
  cat(as.character(SF10_tmp$log_msg), "\n  ", file=out_file_log, append=T)
  
  if (length(SF10_tmp$err_msg[1])=="  # "){
    cat(as.character(SF10_tmp$err_msg), "\n  ", file=out_file_log, append=T)
    
  }
}

# Converto 10cm fluxes to 5cm fluxes
SF$SFU5 <- solar_flux_5cm(SF$SFU10)

# Define reference fluxes for all cases
SFU <- list("3P"=list("1day"=round(SF[as.character(date1_obj), "SFU5"],2), 
                      "3day"=round(median(SF$SFU5, na.rm=TRUE),2)),
            "5P"=list("1day"=round(SF[as.character(date1_obj), "SFU5"],2), 
                      "3day"=round(median(SF$SFU5, na.rm=TRUE),2)))

## INPUT, OUTPUT and DATA ##
out_files <- list()
out_pics <- list()
intfs <- list()
form_res <- list()
inv_warn <- list()
cont <- list()
pointing <- list()

for (i in invs){
  
  out_files[[i]] <- list()
  out_pics[[i]] <- list()
  intfs[[i]] <- list()
  form_res[[i]] <- list()
  inv_warn[[i]] <- list()
  cont[[i]] <- list()
  pointing[[i]] <-list()
  
  for (t in types){
    out_files[[i]][[t]] <- paste(out_path, "/Calib_", i, "_", t, "_", year1, month1, "_", radar,".txt", sep="")
    out_pics[[i]][[t]] <- paste(plot_path, "ScatterFit_",  i, "_", t, "_", radar, ".png", sep="")
    form_res[[i]][[t]] <- data.frame(date=date1_obj, z_cal=NA, delta_zcal=NA, n_intf=0, dx_nom=NA, dy_nom=NA, 
                                     l_scan_nom=NA, p0=NA, sd_p0=NA, x0=NA, sd_x0=NA, y0=NA, sd_y0=NA, dx=NA, sd_dx=NA,
                                     dy=NA, sd_dy=NA, rmse=NA, r_sq=NA, sfu_drao=NA, pow_drao=NA)
    intfs[[i]][[t]] <- data.frame()
    inv_warn[[i]][[t]] <- ""
    pointing[[i]][[t]] <- data.frame(x0=NA, y0=NA)
  } 
}

ava_files <-in_files[file.exists(in_files)]

cat("# Available input files:\n    ", file=out_file_log, append=T)
cat(ava_files, sep="\n    ", file=out_file_log, append=T)

#########################################################################################
#####  SCRIPT CORE  #####################################################################
#########################################################################################

if (file.exists(in_files[[1]])){
  
  n_lines <- length(readLines(in_files[[1]]))
  
  if (n_lines > (hdr_length + 1)){
    
    # Open connection
    conn <- file(in_files[[1]], "r")
    
    # Retrieve header info
    hdr <- readLines(conn, n=hdr_length)
    
    z_cal <- read_num_header(hdr[grep("Calib. cnt", hdr)])
    radar_cnt <- read_num_header(hdr[grep("Radar cnt", hdr)])
    radar_wl <- read_num_header(hdr[grep("Radar wavelength", hdr)])
    rec_bw <- read_num_header(hdr[grep("Receiver bandwidth", hdr)])
    d_beam_nom_h <- read_num_header(hdr[grep("Beam width H", hdr)])
    d_beam_nom_v <- read_num_header(hdr[grep("Beam width V", hdr)])
    d_ray_nom <- read_num_header(hdr[grep("Radial resolution", hdr)])
    
    # Approximation for the effective convolution widths:
    d_conv_nom_h <- d_beam_nom_h + 0.05
    d_conv_nom_v <- d_beam_nom_v + 0.05
    
    # Effective scanning width and scanning losses (from default parameters)
    l_scan_nom <- l_scan_calc(d_ray_nom, d_sun, d_beam_nom_h, d_conv_nom_h)
    d_conv_eff <- solar_widths_scan(d_conv_nom_h, d_conv_nom_v, d_ray_nom)$h
    
    close(conn)
    rm(conn)
    
    for (f in ava_files){
      
      line_num <- length(readLines(f))
      
      # read input file
      if (line_num > (hdr_length + 1)){
        
        # Read input file (current date)
        intfs_tmp <- read.table(file=f, sep= " ", header=T, comment.char = "#")
        intfs_tmp$date <- as.Date(intfs_tmp$date)
        
        intfs_tmp <- intfs_tmp[complete.cases(intfs_tmp),]
        
        intfs_tmp <- intfs_tmp[intfs_tmp$label=="solar",]
        
        intfs_tmp$date <- as.Date(intfs_tmp$date, format="%Y/%m/%d")
        intfs[["3P"]][["3day"]] <- rbind(intfs[["3P"]][["3day"]], intfs_tmp)
        
      }
      
    }
    
    intfs[["5P"]][["3day"]] <- intfs[["3P"]][["3day"]]
    intfs[["3P"]][["1day"]] <- intfs[["3P"]][["3day"]][intfs[["3P"]][["3day"]]$date==date1_obj,]
    intfs[["5P"]][["1day"]] <- intfs[["3P"]][["3day"]][intfs[["3P"]][["3day"]]$date==date1_obj,]
    
    sfu_to_pow <- sfu_to_mw(rec_bw, radar_wl, gain_ant)
    
    for (i in invs){
      
      for (t in types){
        
        pow_drao <- round(10*log10(sfu_to_pow*SFU[[i]][[t]]),2)
        form_res[[i]][[t]]$dx_nom <- round(d_conv_eff, 2)
        form_res[[i]][[t]]$dy_nom <- round(d_conv_nom_v, 2)
        form_res[[i]][[t]]$l_scan_nom <- round(l_scan_nom, 2)
        form_res[[i]][[t]]$sfu_drao <- SFU[[i]][[t]]
        form_res[[i]][[t]]$pow_drao <- pow_drao
        form_res[[i]][[t]]$z_cal <- round(z_cal,2)
        form_res[[i]][[t]]$delta_zcal <- form_res[[i]][[t]]$z_cal-zcal_ref
        form_res[[i]][[t]]$n_intf <- nrow(intfs[[i]][[t]])
        
        ##### INVERSIONS ################################################################
        
        intfs[[i]][[t]]$z <- intfs[[i]][[t]]$pow + intfs[[i]][[t]]$l_gas
        intfs[[i]][[t]] <- subset(intfs[[i]][[t]], select=c(date, daz_x, del_y, z))
        
        res <- sun_fit[[i]](intfs[[i]][[t]]$daz_x, intfs[[i]][[t]]$del_y, intfs[[i]][[t]]$z, 
                            d_conv_nom_h, d_conv_nom_v, d_ray_nom)
        
        cat("  # INVERSION ", i, "-", t , ": ",  as.character(res$log_msg), "\n", sep="", 
            file=out_file_log, append=T)
        cat("    Output file: ", out_files[[i]][[t]], "\n", sep="", file=out_file_log, append=T)
        
        inv_warn[[i]][[t]] <- res$warn
        coefs <- subset(res, select=c(a1, a2, b1, b2, c))
        res_sub <- subset(res, select=-c(a1, a2, b1, b2, c, log_msg, warn))
        form_res[[i]][[t]][, 8:19] <- res_sub
        
        cont[[i]][[t]] <- outer(xf, yf, function(x,y) coefs$a1*(x^2)+coefs$b1*x+coefs$c+coefs$a2*(y^2)+coefs$b2*y) 
        
        pointing[[i]][[t]]$x0 <- form_res[[i]][[t]]$x0
        pointing[[i]][[t]]$y0 <- form_res[[i]][[t]]$y0
        
      }
      
    }
    
  }else{
    cat("  # WARN: INVERSIONS NOT PERFORMED, no data in main input file\n", file=out_file_log, append=T)
    for (i in invs){
      for (t in types){
        inv_warn[[i]][[t]] <- "No data"
      }
    }    
  }
  
  cat("  # SCATTER PLOTS for ", as.character(date1_obj), ":\n", file=out_file_log, sep="", append=T)
  
  for (i in invs){
    
    for (t in types){
      
      H1 <- "# MONTHLY CALIBRATION RESULTS "
      H2 <- paste("# File created:", time_now, sep=" ")
      H3 <- paste("# Radar abbrv.:", radar, sep=" ")
      
      ##### WRITE OUTPUT FILE #########################################################
      
      of <- out_files[[i]][[t]]
      
      # Format data frame values
      form_res[[i]][[t]][, 2:3] <- sprintf("%.2f", round(unlist(form_res[[i]][[t]][, 2:3]),2))
      form_res[[i]][[t]][, 5:20] <- sprintf("%.2f", round(unlist(form_res[[i]][[t]][, 5:20]),2))
      
      if (!file.exists(of)) {
        H4 <- paste("# Model: ", i, sep="")
        H5 <- paste("# Window: ", t, sep="")
        header <- c(H1, H2, H3, H4, H5, rep("#", 10))
        write(header, file=of, ncolumns=14, sep="\n", append=T)
        write.table(form_res[[i]][[t]], file=of, append=T, quote=FALSE, row.names=FALSE, col.names=TRUE)
        
      }else{
        write.table(form_res[[i]][[t]], file=of, append=T, quote=FALSE, row.names=FALSE, col.names=FALSE)
      }
      
      ##### SCATTER PLOT #################################################################
      
      out_plot <- out_pics[[i]][[t]]
      cat("    ", out_plot, "\n", sep="", file=out_file_log, append=T)
      
      int_plt <- intfs[[i]][[t]]
      con_plt <- cont[[i]][[t]]
      
      x0 <- pointing[[i]][[t]]$x0
      y0 <- pointing[[i]][[t]]$y0
      r_sq <- form_res[[i]][[t]]$r_sq
      Nint <- form_res[[i]][[t]]$n_intf
      dx_nom <- as.numeric(form_res[[i]][[t]]$dx_nom)
      dy_nom <- as.numeric(form_res[[i]][[t]]$dy_nom)
      dx <- as.numeric(form_res[[i]][[t]]$dx)
      dy <- as.numeric(form_res[[i]][[t]]$dy)
      WarnMSG <- inv_warn[[i]][[t]]
      
      tit <- paste(radar, " ", date1_obj, ", Intf. scatter : ",  i, " - ", t, sep="") 
      
      # Axes for setting min(x,y) that ensures sensitivity
      cnt <- 5/(80*log10(2))
      axx_width <- sqrt(cnt*(dx_nom^3))
      axy_width <- sqrt(cnt*(dy_nom^3))
      axx_point <- cnt*(dx_nom^2)
      axy_point <- cnt*(dy_nom^2)
      
      if (nrow(int_plt)==0){
        int <- data.frame()  
      }else{
        
        # Assign color to interference power values
        int_plt$ColorP <- NA
        int_plt[int_plt$z < brks_int[2], "ColorP"] <- cols[1]
        for (j in seq(2, (length(brks_int)-1))){
          int_plt[int_plt$z>=brks_int[j] & int_plt$z<brks_int[j+1], "ColorP"] <- cols[j]
        }
        int_plt[int_plt$z >= brks_int[length(brks_int)], "ColorP"] <- cols[length(cols)]
        
      }
      
      png(file=out_plot, bg="transparent", height=1500, width=1500)
      layout(matrix(c(1,2), 2,1), heights=c(3,1))
      par(mar=c(10, 10, 8, 10), mgp=c(5,1,0), pty="s")
      plot(x0, y0, pch="+", cex=4.5, lwd=2.5,
           xlim=lims, ylim=lims, xaxs="i", yaxs="i",
           ylab="Elevation distance R-S [deg]",
           main=tit, cex.main=3,
           xaxt="n", xlab="", cex.lab=3, cex.axis=3)
      rect(xleft = x0-c(axx_width, axx_point), xright = x0+c(axx_width, axx_point), 
           ybottom = y0-c(axy_width, axy_point), ytop = y0+c(axy_width, axy_point),
           border=NA, col=c("grey90", "white"), lty = c(2,1))
      axis(1, padj=0.5, cex.lab=3, cex.axis=3)
      grid(nx = length(minor)-1, ny = length(minor)-1, col=col_minor, lwd=2, lty=2 )
      abline(v=0, col="grey45", lwd=2.5, lty=1)
      abline(h=0, col="grey45", lwd=2.5, lty=1)
      mtext("Azimuth distance R-S [deg]",
            side=1, line=6, cex=3)
      text(0,0, adj=c(0.5,NA), labels=WarnMSG, cex=6, col="black")
      
      if (!is.null(con_plt)){
        par(new=T)
        contour(x=xf, y=yf, z=con_plt, xaxt="n", yaxt="n", levels=brks_cnt, col=cols,
                lwd=4.5, labcex=2.5, vfont=c("sans serif", "bold"), xlim=lims, ylim=lims)
        text(0.9, 1.05, adj=c(0.5,NA), labels=paste("Rsq=", r_sq, sep=""), cex=3, col="black")
        text(0.9, 0.9, adj=c(0.5,NA), labels=paste("N=", Nint, sep=""), cex=3, col="black")
      }
      
      points(int_plt$daz_x, int_plt$del_y, col=int_plt$ColorP, 
             alpha=0.8, pch=16, cex=4.5, lwd=3)
      points(x0, y0, pch="+", cex=3.5, lwd=2.5)
      
      plot.new()
      color.legend(5.5, -0.1, -4.5, 0.5, rect.col=rev(cols_names), 
                   legend=rev(labs), bg="transparent", 
                   cex=2)
      title(main="Corrected Power [dBm]", cex.main=2.5, line=-0.5)
      
      garbage <- dev.off()
      
      
      
      
      
    }
    
  }
  
}else{
  cat("  # INVERSIONS NOT PERFORMED, main input file not found\n", file=out_file_log, append=T)
  cat("    WARN: NO OUTPUT PLOTS GENERATED\n", sep="", file=out_file_log, append=T)
  
}

##### EOS ###############################################################################
