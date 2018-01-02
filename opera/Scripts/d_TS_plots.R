#!/usr/bin/env Rscript

# Program in R - Time series plots of calibration parameters
# P. Altube Vazquez - July 2017

#########################################################################################
##### INFO and USAGE ####################################################################

# This script has been developed for its automatic and daily execution within the solar 
# monitoring project (SunINTFCal). The shell script that boots up the daily proccess is: 
# "SunINTFCal_daily.sh"
#                                                                                    
# This script loads the calibration parameters (input files: "Calib_MP_Nday_YYMM_XXX.txt")
# and generates graphical output in time-series format:
# Power (relative): PTOA_MP_Nday_YYMMDD_XXX.png
# Power (absolute): PowerDiff_MP_Nday_YYMMDD_XXX.png
# Pointing bias: Pointing_MP_Nday_YYMMDD_XXX.png
# Quality of fit: Qfit_Rsq_Nday_YYMMDD_XXX.png ; Qfit_RMSE_Nday_YYMMDD_XXX.png                                                                #
#         
# Command line execution: specify time series start and end date, radar abbrv. and workpath:                                                                         # 
# Rscript .../d_TS_plots.R YYMMDDst YYMMDDend XXX ...
# If no date information is required in the names of the output figures add "NODATE" as
# a last command line argument: 
# Rscript .../d_TS_plots.R YYMMDDst YYMMDDend XXX ... NODATE  

#########################################################################################
##### LIBRARIES #########################################################################

options(warn=-1)

library(getopt, quietly=T)
library(ggplot2, quietly=T)
library(grid, quietly=T)
library(scales, quietly=T)
library(plyr, quietly=T)

time_now <- Sys.time()

#########################################################################################
##  FUNCTIONS  ###########################################################################

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

as.Date_origin <- function(x){
  format(as.Date(x, origin = "1970-01-01"), format="%b %d")
}

#########################################################################################
##### DEFAULT SETTINGS ##################################################################

# Date, radar abbrv. and input .tgz file if not provided as command line arguments:
date_str_i <- "170530"
date_str_f <- "170730"

radar <- "PBE"

work_path <- "/home/operator/progR/SunINTFCal"

#########################################################################################
##  SETTINGS  ###########################################################################

# Number of header lines in the input file:
hdr_length <- 15

#########################################################################################
##  PARAMETERS  #########################################################################

# Name of the pictures to be generated
ptoa_name <- "PTOA"
pointing_name <- "Pointing"
width_name <- "Widths"
power_name <- "PowerDiff"
rsq_name <- "Qfit_Rsq"
rmse_name <- "Qfit_RMSE"

pow_lims <- c(-116, -104)
d_pow_lims <- c(-3.5, 3.5)
point_lims <- c(-0.6, 0.6)
width_lims <- c(0.4, 2)
rsq_lims <- c(-0.05, 1.05)
rmse_lims <- c(0, 1.4)

types <- c("1day", "3day")
invs <- c("3P", "5P")

#########################################################################################
##### ARGUMENTS #########################################################################

#Retrieve command-line argument(s): Date and radar name
options <- commandArgs(trailingOnly=T)

if (!is.na(options[1])){
  date_str_i <- options[1]
}
if (!is.na(options[2])){
  date_str_f <- options[2]
}
if (!is.na(options[3])){
  radar <- options[3]
}
if (!is.na(options[4])){
  work_path <- options[4]
}
if (!is.na(options[5]) & options[5]=="NODATE"){
  date_bool <- F
}else{
  date_bool <- T
}

#########################################################################################
##### VARIABLES #########################################################################

# Convert input dates to Date class objects:
date_obj_i<-as.Date(date_str_i, format="%y%m%d")
date_obj_f<-as.Date(date_str_f, format="%y%m%d")

# Sequence of dates delimited by the input-date pair:
date_obj_seq <- seq(date_obj_i, date_obj_f, 1)
ym_int <- unique(format(c(min(date_obj_seq), max(date_obj_seq)), format="%b %Y"))

# Collection of Year-Month pairs within the sequence:
year_month_seq <- unique(format(date_obj_seq, format="%y%m"))
year_long_seq <- unique(format(date_obj_seq, format="%Y"))
month_seq <- unique(format(date_obj_seq, format="%b"))

# LOG file:
log_path <- paste(work_path, radar, "LOG/", sep="/")
out_file_log <- paste(log_path, "LOGsunINTF_", date_str_f, "_", radar, ".txt", sep="")
if (!file.exists(log_path)){dir.create(log_path, showWarnings =F, recursive = T)}

# LOG file: execution message
cat("\n# EXECUTION: d_TS_plots.R\n", "  ", as.character(time_now), "\n",
    file=out_file_log, append=T, sep="")
cat("\n# EXECUTION: d_TS_plots.R ", as.character(time_now), "\n", sep="")

cat("  # Creating time-series plots: ", format(date_str_i, format="%y%m%d"), " to ", format(date_str_f, format="%y%m%d"),  "\n", 
    file=out_file_log, sep="", append=T)

# Outpath and output pictures:
out_path <- paste(work_path, radar, "Plots/", sep="/")
if (!file.exists(out_path)){dir.create(out_path, showWarnings =F, recursive = T)}

# Input path and input files
in_path <- paste(work_path, radar, sep="/")

# Data for drawing horizontal lines crossing 0:
data_hline0 <- data.frame(x=seq(date_obj_i-5, date_obj_f+5, 1), y=rep(0, length(date_obj_seq)+10))
data_diag <- data.frame(x=seq(-0.5, 1.5, 0.1), y=seq(-0.5, 1.5, 0.1))

#########################################################################################
#####  SCRIPT CORE  #####################################################################
#########################################################################################

for (t in types){
  
  cat("  # ", t, "\n", file=out_file_log, sep="", append=T)
  
  data_all <- list()
  
  if (date_bool){
    rsq_pic <- paste(out_path, rsq_name, "_", t, "_", date_str_f, "_", radar, ".png", sep="")
    rmse_pic <- paste(out_path, rmse_name, "_", t, "_", date_str_f, "_", radar, ".png", sep="")
  }else{
    rsq_pic <- paste(out_path, rsq_name, "_", t, "_", radar, ".png", sep="")
    rmse_pic <- paste(out_path, rmse_name, "_", t, "_", radar, ".png", sep="")
  }
  
  for (i in invs){
    
    if (date_bool){
      
      pointing_pic <- paste(out_path, pointing_name, "_", i, "_", t, "_", date_str_f, "_", radar, ".png", sep="")
      width_pic <- paste(out_path, width_name, "_", i, "_", t, "_", date_str_f, "_", radar, ".png", sep="")
      power_pic <- paste(out_path, power_name, "_", i, "_", t, "_", date_str_f, "_", radar, ".png", sep="")
      ptoa_pic  <- paste(out_path, ptoa_name, "_", i, "_", t, "_", date_str_f, "_", radar, ".png", sep="")
      
    }else{
      
      pointing_pic <- paste(out_path, pointing_name, "_", i, "_", t, "_", radar, ".png", sep="")
      width_pic <- paste(out_path, width_name, "_", i, "_", t, "_", radar, ".png", sep="")
      power_pic <- paste(out_path, power_name, "_", i, "_", t, "_", radar, ".png", sep="")
      ptoa_pic <- paste(out_path, ptoa_name, "_", i, "_", t, "_", radar, ".png", sep="")
      
    }
    
    data_all[[i]] <- data.frame()
    
    cat("    ", i, " INPUT:\n", file=out_file_log, sep="", append=T)
    
    in_files <- c()
    for (ym in year_month_seq){
      in_files <- c(in_files, paste(in_path, "/Calib_",  i, "_", t, "_", ym, "_", radar, ".txt", sep=""))
    }
    in_files <- in_files[file.exists(in_files)]
    
    if (length(in_files)!=0){
      
      for (in_file in in_files){
        
        # LOG file: input file messages
        cat("    ", in_file, file=out_file_log, sep="", append=T)
        
        line_num <- length(readLines(in_file))
        
        # LOG file warning
        cat("    FILE OK", "\n", file=out_file_log, append=T, sep="")
        
        if (line_num > (hdr_length+1)){
          
          res <- read.table(file = in_file, skip = hdr_length, header = T)
          data_all[[i]] <- rbind(data_all[[i]], res)
          
        }else{
          
          # LOG file warning
          cat("    NO DATA IN FILE", "\n", file=out_file_log, append=T, sep="")
          
        }
        
      }  
        
      data_all[[i]]$date <- as.Date(as.character(data_all[[i]]$date))
      
      data_all[[i]] <- data_all[[i]][data_all[[i]]$date>=date_obj_i,]
      data_all[[i]] <- data_all[[i]][data_all[[i]]$date<=date_obj_f,]
      
      data_all[[i]] <- data_all[[i]][order(data_all[[i]][,"date"]), ]
      
      # Power [dBm] units (scaled for scanning losses)
      data_all[[i]]$pow <- data_all[[i]]$p0 - data_all[[i]]$l_scan_nom
      
      # Negative Rsq values converted to 0 for plotting:
      data_all[[i]]$r_sq[data_all[[i]]$r_sq<0] <- 0
      data_all[[i]]$delta_pow <- data_all[[i]]$pow - data_all[[i]]$pow_drao
      data_all[[i]]$inv <- i
      
      cat("    ", i, " OUTPUT:\n", file=out_file_log, sep="", append=T)
      
      plot_tit <- paste(radar, " ", paste(ym_int, collapse="-"), ": ", i, " - ", t, sep="")
      qfit_tit <- paste(radar, " ", paste(ym_int, collapse="-"), ": ", t, sep="")
      
      ## POWER RELATIVE PLOT #########################################################################
      
      # Comparison of the solar power at the TOA [dB] as estimated from inversion with the DRAO
      # observatory reference
      
      # Workaround to avoid problems when all values are NA
      nrow_pow <- nrow(data_all[[i]][complete.cases(data_all[[i]][, c("pow", "pow_drao")]), ])
      if (nrow_pow==0){
        data_pow <- data.frame(date=date_obj_seq, pow=0)
      }else{
        data_pow <- data_all[[i]]
      }
      
      p_pow <- ggplot(data=data_pow, aes(x=date, y=pow))+        
        scale_x_date(date_breaks = "7 day", date_labels = "%b %d", date_minor_breaks = "1 day") +
        scale_y_continuous(breaks=seq(pow_lims[1]+1, pow_lims[2]-1, 1),
                           labels=plot_labels(seq(pow_lims[1]+1, pow_lims[2]-1, 5), seq(pow_lims[1]+1, pow_lims[2]-1, 1), ndec=0),
                           expand = c(0,0)) +
        coord_cartesian(ylim=pow_lims, xlim=c(date_obj_i, date_obj_f)) + theme_bw() +
        xlab(paste(year_long_seq, collapse = " - ")) + ylab("Solar power at TOA [dBm]") + 
        ggtitle(plot_tit) + 
        theme(axis.text=element_text(size=20),
              axis.text.x=element_text(angle=0),
              axis.title.y=element_text(size=22, vjust=0.5),
              axis.title.x=element_text(size=24, margin = margin(t = 20)),
              axis.ticks=element_line(colour="grey10"),
              axis.ticks.length = unit(0.2, "cm"),
              panel.border = element_rect(size=0.8, colour="black"),
              panel.margin = unit(0.7, "lines"),
              panel.background = element_rect(fill = "transparent", colour = NA),
              panel.grid.major = element_line(size=0.2, colour="grey80"),
              panel.grid.minor = element_line(size=0.2, colour="grey80", linetype=2),
              plot.title = element_text(lineheight=.3, face="bold", hjust = 0.5, size=22),
              plot.background = element_rect(fill = "transparent", colour = NA),
              plot.margin=unit(c(1,1,1,1), "cm"),
              legend.position = "top",
              legend.background = element_rect(fill = "transparent", colour = NA),
              legend.key = element_rect(colour = NA, fill = NA),
              legend.key.size = unit(1, "cm"),
              legend.text = element_text(size=22))
      
      if (nrow_pow!=0){
        p_pow <- p_pow + 
          geom_errorbar(data=data_pow, aes(x=date, ymin=pow-sd_p0, ymax=pow+sd_p0, colour = "Estimate"), size=3, alpha=0.2, width=0)+
          geom_point(data=data_pow, aes(x=date, y=pow, colour = "Estimate", shape = "Estimate", size = "Estimate"), alpha=0.6) + 
          geom_line(data=data_pow, aes(x=date, y=pow, colour="Estimate", lty="Estimate")) +
          geom_point(data=data_pow, aes(x=date, y=pow_drao, colour="DRAO", shape = "DRAO", size = "DRAO"), alpha=0.6) + 
          geom_line(data=data_pow, aes(x=date, y=pow_drao, colour="DRAO", lty="DRAO")) +
          scale_linetype_manual("", values = c("Estimate"=1, "DRAO"=1)) +
          scale_size_manual("", values = c("Estimate"=4, "DRAO"=4)) +
          scale_shape_manual("", values = c("Estimate"=16, "DRAO"=16)) +
          scale_colour_manual("", values = c("Estimate"="darkred", "DRAO"="grey10"))
          
      }
      
      png(file=ptoa_pic, bg="transparent",  width = 1000, height =700)
      print(p_pow)
      garbage <- dev.off()
      
      cat("    ", ptoa_pic,  "\n", file=out_file_log, sep="", append=T)
      
      
      ## POINTING BIAS PLOT ###################################################################
      
      # Plots of the estimated systematic antenna pointing biases in azimuth and elevation
      
      # point_tit <- paste(radar, " ", paste(ym_int, collapse="-"), ", Pointing bias: ", i, " - ", t, sep="")
      
      # Workaround to avoid problems when all values are NA
      nrow_point <-  nrow(data_all[[i]][complete.cases(data_all[[i]][, c("x0", "y0")]), ])
      if (nrow_point==0){
        data_point <- data.frame(date=date_obj_seq, x0=0)
      }else{
        data_point <- data_all[[i]]
      }
      
      p_point <- ggplot(data=data_point, aes(x=date, y=x0))+
        geom_line(data=data_hline0, aes(x=x, y=y), linetype=2, size=0.5, colour="black") +
        scale_x_date(date_breaks = "7 day", date_labels = "%b %d", date_minor_breaks = "1 day") +
        scale_y_continuous(breaks=seq(point_lims[1]+0.1, point_lims[2]-0.1, 0.1),
                           labels=plot_labels(seq(point_lims[1], point_lims[2], 0.2), seq(point_lims[1]+0.1, point_lims[2]-0.1, 0.1), ndec=1),
                           expand = c(0,0)) +
        coord_cartesian(ylim=point_lims, xlim=c(date_obj_i, date_obj_f)) + theme_bw() +
        xlab(paste(year_long_seq, collapse = " - ")) + ylab("Antenna pointing bias [deg]") + 
        ggtitle(plot_tit) + 
        theme(axis.text=element_text(size=20),
              axis.text.x=element_text(angle=0),
              axis.title.y=element_text(size=22, vjust=0.5),
              axis.title.x=element_text(size=24, margin = margin(t = 20)),
              axis.ticks=element_line(colour="grey10"),
              axis.ticks.length = unit(0.2, "cm"),
              panel.border = element_rect(size=0.8, colour="black"),
              panel.margin = unit(0.7, "lines"),
              panel.background = element_rect(fill = "transparent", colour = NA),
              panel.grid.major = element_line(size=0.2, colour="grey80"),
              panel.grid.minor = element_line(size=0.2, colour="grey80", linetype=2),
              plot.title = element_text(lineheight=.3, face="bold", hjust = 0.5, size=22),
              plot.background = element_rect(fill = "transparent", colour = NA),
              plot.margin=unit(c(1,1,1,1), "cm"),
              legend.position = "top",
              legend.background = element_rect(fill = "transparent", colour = NA),
              legend.key = element_rect(colour = NA, fill = NA),
              legend.key.size = unit(1, "cm"),
              legend.text = element_text(size=22))
      
      if (nrow_point!=0){
        p_point <- p_point + 
          geom_errorbar(data=data_point, aes(x=date, ymin=x0-sd_x0, ymax=x0+sd_x0, colour = "Azimuth"), size=3, alpha=0.2, width=0)+
          geom_errorbar(data=data_point, aes(x=date, ymin=y0-sd_y0, ymax=y0+sd_y0, colour = "Elevation"), size=3, alpha=0.2, width=0)+
          geom_point(data=data_point, aes(x=date, y=x0, colour = "Azimuth", fill = "Azimuth", shape = "Azimuth", size = "Azimuth"), alpha=0.6) + 
          geom_point(data=data_point, aes(x=date, y=y0, colour="Elevation", fill="Elevation", shape = "Elevation", size = "Elevation"), alpha=0.6) + 
          geom_line(data=data_point, aes(x=date, y=x0, colour = "Azimuth")) +
          geom_line(data=data_point, aes(x=date, y=y0, colour="Elevation")) +
          scale_size_manual("", values = c("Azimuth"=4, "Elevation"=4)) +
          scale_shape_manual("", values = c("Azimuth"=21, "Elevation"=25)) +
          scale_colour_manual("", values = c("Azimuth"="darkgreen", "Elevation"="darkblue")) +
          scale_fill_manual("", values = c("Azimuth"="darkgreen", "Elevation"="darkblue"))
          
      }
      
      png(file=pointing_pic, bg="transparent",  width = 1000, height =700)
      print(p_point)
      garbage <- dev.off()
      
      cat("    ", pointing_pic,  "\n", file=out_file_log, sep="", append=T)
      
      
      ## POWER ABS. PLOT ####################################################################
      
      # Plots of the estimated power (at TOA) difference with respect to the reference, 
      # in comparison with the value of the calibration constant
      
      # Dpow_tit <- paste(radar, " ", paste(ym_int, collapse="-"), ", Power calib. (absolute): ", i, " - ", t, sep="")
      
      # Workaround to avoid problems when all values are NA
      nrow_dpow <- nrow(data_all[[i]][complete.cases(data_all[[i]][, c("delta_pow", "delta_zcal")]), ])
      if (nrow_dpow==0){
        data_dpow <- data.frame(date=date_obj_seq, delta_pow=0, sd_p0=0, delta_zcal=0)
      }else{
        data_dpow <- data_all[[i]]
      }
      
      p_dpow <- ggplot(data=data_dpow, aes(x=date, y=delta_pow))+
        geom_line(data=data_hline0, aes(x=x, y=y), linetype=2, size=0.5, colour="black") +
        scale_x_date(date_breaks = "7 day", date_labels = "%b %d", date_minor_breaks = "1 day") +
        scale_y_continuous(breaks=seq(d_pow_lims[1]+0.5, d_pow_lims[2]-0.5, 0.5),
                           labels=plot_labels(seq(d_pow_lims[1]+0.5, d_pow_lims[2]-0.5, 1), seq(d_pow_lims[1]+0.5, d_pow_lims[2]-0.5, 0.5), ndec=1),
                           expand = c(0,0)) +
        coord_cartesian(ylim=d_pow_lims, xlim=c(date_obj_i, date_obj_f)) + theme_bw() +
        xlab(paste(year_long_seq, collapse = " - ")) + ylab("Difference with respect to reference [dB]") + 
        ggtitle(plot_tit) + 
        theme(axis.text=element_text(size=20),
              axis.text.x=element_text(angle=0),
              axis.title.y=element_text(size=22, vjust=0.5),
              axis.title.x=element_text(size=24, margin = margin(t = 20)),
              axis.ticks=element_line(colour="grey10"),
              axis.ticks.length = unit(0.2, "cm"),
              panel.border = element_rect(size=0.8, colour="black"),
              panel.margin = unit(0.7, "lines"),
              panel.background = element_rect(fill = "transparent", colour = NA),
              panel.grid.major = element_line(size=0.2, colour="grey80"),
              panel.grid.minor = element_line(size=0.2, colour="grey80", linetype=2),
              plot.title = element_text(lineheight=.3, face="bold", hjust = 0.5, size=22),
              plot.background = element_rect(fill = "transparent", colour = NA),
              plot.margin=unit(c(1,1,1,1), "cm"),
              legend.position = "top",
              legend.background = element_rect(fill = "transparent", colour = NA),
              legend.key = element_rect(colour = NA, fill = NA),
              legend.key.size = unit(1, "cm"),
              legend.text = element_text(size=22))
      
      if (nrow_dpow!=0){
        p_dpow <- p_dpow +
          geom_errorbar(data=data_dpow, aes(x=date, ymin=delta_pow-sd_p0, ymax=delta_pow+sd_p0, colour = "Power-DRAO"), size=3, alpha=0.2, width=0)+
          geom_point(data=data_dpow, aes(x=date, y=delta_pow, colour = "Power-DRAO", fill = "Power-DRAO", shape = "Power-DRAO", size = "Power-DRAO"), alpha=0.6) +
          geom_line(data=data_dpow, aes(x=date, y=delta_pow, colour = "Power-DRAO")) +
          geom_line(data=data_dpow, aes(x=date, y=delta_zcal, colour="Zcal-(-40dBZ)"), size=0.5) +
          geom_point(data=data_dpow, aes(x=date, y=delta_zcal, colour="Zcal-(-40dBZ)", fill="Zcal-(-40dBZ)", shape = "Zcal-(-40dBZ)", size = "Zcal-(-40dBZ)"), alpha=0.6) + 
          scale_size_manual("", values = c("Power-DRAO"=4, "Zcal-(-40dBZ)"=3.5)) +
          scale_shape_manual("", values = c("Power-DRAO"=24, "Zcal-(-40dBZ)"=24)) +
          scale_colour_manual("", values = c("Power-DRAO"="darkred", "Zcal-(-40dBZ)"="grey10")) +
          scale_fill_manual("", values = c("Power-DRAO"="darkred", "Zcal-(-40dBZ)"="grey10"))
        
      }
      
      png(file=power_pic, bg="transparent",  width = 1000, height =700)
      print(p_dpow)
      garbage <- dev.off()
      
      cat("    ", power_pic,  "\n", file=out_file_log, sep="", append=T)
      
      if (i=="5P"){
        
        ## BEAMWIDTHS PLOT ######################################################################
        
        # Plots of the estimated effective beamwidths in azimuth and elevation
        
        # width_tit <- paste(radar, " ", paste(ym_int, collapse="-"), ", Solar image widths: ", i, " - ", t, sep="")
        
        # Workaround to avoid problems when all values are NA
        nrow_width <- nrow(data_all[[i]][complete.cases(data_all[[i]][, c("dx", "dx")]), ])
        if (nrow_width==0){
          data_width <- data.frame(date=date_obj_seq, dx=0, dy=0, sd_dx=0, sd_dy=0)
        }else{
          data_width <- data_all[[i]]
        }
        
        p_width <- ggplot(data=data_width, aes(x=date, y=dx))+
          coord_cartesian(ylim=width_lims, xlim=c(date_obj_i, date_obj_f)) + theme_bw() +
          scale_x_date(date_breaks = "7 day", date_labels = "%b %d", date_minor_breaks = "1 day") +
          scale_y_continuous(breaks=seq(width_lims[1]+0.1, width_lims[2]-0.1, 0.1),
                             labels=plot_labels(seq(width_lims[1], width_lims[2], 0.2), seq(width_lims[1]+0.1, width_lims[2]-0.1, 0.1), ndec=1),
                             expand = c(0,0)) +
          xlab(paste(year_long_seq, collapse = " - ")) + ylab("Solar image width [deg]") + 
          ggtitle(plot_tit) + 
          theme(axis.text=element_text(size=20),
                axis.text.x=element_text(angle=0),
                axis.title.y=element_text(size=22, vjust=0.5),
                axis.title.x=element_text(size=24, margin = margin(t = 20)),
                axis.ticks=element_line(colour="grey10"),
                axis.ticks.length = unit(0.2, "cm"),
                panel.border = element_rect(size=0.8, colour="black"),
                panel.margin = unit(0.7, "lines"),
                panel.background = element_rect(fill = "transparent", colour = NA),
                panel.grid.major = element_line(size=0.2, colour="grey80"),
                panel.grid.minor = element_line(size=0.2, colour="grey80", linetype=2),
                plot.title = element_text(lineheight=.3, face="bold", hjust = 0.5, size=22),
                plot.background = element_rect(fill = "transparent", colour = NA),
                plot.margin=unit(c(1,1,1,1), "cm"),
                legend.position = "top",
                legend.background = element_rect(fill = "transparent", colour = NA),
                legend.key = element_rect(colour = NA, fill = NA),
                legend.key.size = unit(1, "cm"),
                legend.text = element_text(size=22))
        
        if (nrow_width!=0){
          p_width <- p_width +
            geom_errorbar(data=data_width, aes(x=date, ymin=dx-sd_dx, ymax=dx+sd_dx, colour = "Azimuth"), size=3, alpha=0.15, width=0)+
            geom_errorbar(data=data_width, aes(x=date, ymin=dy-sd_dy, ymax=dy+sd_dy, colour = "Elevation"), size=3, alpha=0.15, width=0)+
            geom_point(data=data_width, aes(x=date, y=dx, colour = "Azimuth", fill = "Azimuth", shape = "Azimuth", size = "Azimuth"), alpha=0.6) + 
            geom_point(data=data_width, aes(x=date, y=dy, colour="Elevation", fill="Elevation", shape = "Elevation", size = "Elevation"), alpha=0.6) + 
            geom_line(data=data_width, aes(x=date, y=dx, colour = "Azimuth")) +
            geom_line(data=data_width, aes(x=date, y=dy, colour="Elevation")) +
            geom_line(data=data_width, aes(x=date, y=dy_nom, colour="Elevation"), size=0.65, lty=2) + 
            geom_line(data=data_width, aes(x=date, y=dx_nom, colour="Azimuth"), size=0.65, lty=2) + 
            scale_size_manual("", values = c("Azimuth"=4, "Elevation"=4)) +
            scale_shape_manual("", values = c("Azimuth"=21, "Elevation"=24)) +
            scale_colour_manual("", values = c("Azimuth"="darkgreen", "Elevation"="darkblue")) +
            scale_fill_manual("", values = c("Azimuth"="darkgreen", "Elevation"="darkblue"))
            
        }
        png(file=width_pic, bg="transparent",  width = 1000, height =700)
        print(p_width)
        garbage <- dev.off()
        
        cat("    ", width_pic,  "\n", file=out_file_log, sep="", append=T)
        
      }
      
      cat("    OTHER OUTPUT:\n", file=out_file_log, sep="", append=T)
      
      if (length(data_all)>1){
        data_plot <- Reduce(function(...) merge(..., by="date", suffixes = paste("_", names(data_all), sep="")), data_all)
        nrow_rsq <- nrow(data_plot[complete.cases(data_plot[, c("r_sq_3P", "r_sq_5P")]), ])
        nrow_rmse <- nrow(data_plot[complete.cases(data_plot[, c("rmse_3P", "rmse_5P")]), ])
      }else{
        data_plot <- data.frame()
        nrow_rsq <- 0
        nrow_rmse <- 0
      }
      ## R-SQUARED PLOT #################################################################
      
      # Workaround to avoid problems when all values are NA
      if (nrow_rsq==0){
        data_rsq <- data.frame(date=date_obj_seq, r_sq_5P=0, r_sq_3P=0)
      }else{
        data_rsq <- data_plot
      }
      
      p_rsq <- ggplot(data=data_rsq, aes(x=r_sq_3P, y=r_sq_5P))+
        geom_line(data=data_diag, aes(x=x, y=y), linetype=2, size=0.5, colour="black") +
        coord_fixed(ylim=rsq_lims, xlim=rsq_lims) + theme_bw() +
        scale_y_continuous(breaks=seq(rsq_lims[1]+0.15, rsq_lims[2]-0.15, 0.1), expand = c(0,0)) +
        scale_x_continuous(breaks=seq(rsq_lims[1]+0.15, rsq_lims[2]-0.15, 0.1), expand = c(0,0)) +
        xlab("R-squared (3P)") + ylab("R-squared (5P)") +
        ggtitle(qfit_tit) +
        theme(axis.text=element_text(size=20),
              axis.text.x=element_text(angle=0),
              axis.title.y=element_text(size=22, margin = margin(c(10,10,10,10))),
              axis.title.x=element_text(size=22, margin = margin(c(10,10,10,10))),
              axis.ticks=element_line(colour="grey10"),
              axis.ticks.length = unit(0.2, "cm"),
              panel.border = element_rect(size=0.8, colour="black"),
              panel.margin = unit(0.7, "lines"),
              panel.background = element_rect(fill = "transparent", colour = NA),
              panel.grid.major = element_line(size=0.2, colour="grey80"),
              panel.grid.minor = element_line(size = 0.2, colour = "grey80", linetype=2),
              plot.title = element_text(lineheight=.3, face="bold", hjust = 0.5, size=22),
              plot.background = element_rect(fill = "transparent", colour = NA),
              plot.margin=unit(c(1,1,1,1), "cm"),
              legend.position = "bottom",
              legend.background = element_rect(fill = "transparent", colour = NA),
              legend.key = element_rect(colour = NA, fill = NA),
              legend.key.width = unit(2.6, "cm"),
              legend.key.height = unit(1, "cm"),
              legend.text = element_text(size=16),
              legend.title = element_text(size=17, face="bold"),
              legend.title.align = 0.5)
      
      if (nrow_rsq!=0){
        p_rsq <- p_rsq +
          geom_point(data=data_rsq, aes(x=r_sq_3P, y=r_sq_5P, fill = as.integer(date)), shape = 21, size = 5, alpha=0.7) +
          scale_fill_gradientn(colours = rev(heat.colors(31)), labels=as.Date_origin, name=paste(paste(year_long_seq, collapse = " - "), "date:\n", sep=" "))
      }
      
      png(file=rsq_pic, bg="transparent",  width = 700, height =700)
      print(p_rsq)
      garbage <- dev.off()
      
      cat("    ", rsq_pic,  "\n", file=out_file_log, sep="", append=T)
      
      ## RMSE PLOT ######################################################################
      
      # Workaround to avoid problems when all values are NA
      if (nrow_rmse==0){
        data_rmse <- data.frame(date=date_obj_seq, rmse_5P=0, rmse_3P=0)
      }else{
        data_rmse <- data_plot
      }
      
      p_rmse <- ggplot(data=data_rmse, aes(x=date, y=rmse_3P))+
        scale_x_date(date_breaks = "7 day", date_labels = "%b %d", date_minor_breaks = "1 day") +
        scale_y_continuous(breaks=seq(rmse_lims[1]+0.1, rmse_lims[2]-0.1, 0.1),
                           labels=plot_labels(seq(rmse_lims[1]+0.2, rmse_lims[2]-0.2, 0.2), seq(rmse_lims[1]+0.1, rmse_lims[2]-0.1, 0.1), ndec=1),
                           expand = c(0,0)) +
        coord_cartesian(ylim=rmse_lims, xlim=c(date_obj_i, date_obj_f)) + theme_bw() +
        xlab(paste(year_long_seq, collapse = " - ")) + ylab("RMSE of fit [dB]") + 
        ggtitle(qfit_tit) + 
        theme(axis.text=element_text(size=20),
              axis.text.x=element_text(angle=0),
              axis.title.y=element_text(size=22, vjust=0.5),
              axis.title.x=element_text(size=24, margin = margin(t = 20)),
              axis.ticks=element_line(colour="grey10"),
              axis.ticks.length = unit(0.2, "cm"),
              panel.grid.major = element_line(size=0.2, colour="grey80"),
              panel.grid.minor = element_line(size=0.2, colour="grey80", linetype=2),
              panel.border = element_rect(size=0.8, colour="black"),
              panel.margin = unit(0.7, "lines"),
              panel.background = element_rect(fill = "transparent", colour = NA),
              plot.title = element_text(lineheight=.3, face="bold", hjust = 0.5, size=22),
              plot.background = element_rect(fill = "transparent", colour = NA),
              plot.margin=unit(c(1,1,1,1), "cm"),
              legend.position = "top",
              legend.background = element_rect(fill = "transparent", colour = NA),
              legend.key = element_rect(colour = NA, fill = NA),
              legend.key.size = unit(1, "cm"),
              legend.text = element_text(size=22))
      
      if (nrow_rmse!=0){
    
        p_rmse <- p_rmse +
          geom_point(data=data_rmse, aes(x=date, y=rmse_3P, colour = "3P model", fill = "3P model", shape = "3P model", size = "3P model", alpha="3P model")) + 
          geom_point(data=data_rmse, aes(x=date, y=rmse_5P, colour="5P model", fill="5P model", shape = "5P model", size = "5P model", alpha="5P model")) + 
          geom_line(data=data_rmse, aes(x=date, y=rmse_3P, colour = "3P model"), size=0.5) +
          geom_line(data=data_rmse, aes(x=date, y=rmse_5P, colour="5P model"), size=0.5) +
          scale_size_manual("", values = c("3P model"=4, "5P model"=4)) +
          scale_shape_manual("", values = c("3P model"=21, "5P model"=24)) +
          scale_colour_manual("", values = c("3P model"="dodgerblue3", "5P model"="darkblue")) +
          scale_fill_manual("", values = c("3P model"="dodgerblue3", "5P model"="darkblue")) +
          scale_alpha_manual("", values = c("3P model"=0.6, "5P model"=0.6))
          
      }
      
      png(file=rmse_pic, bg="transparent",  width = 1000, height =700)
      print(p_rmse)
      garbage <- dev.off()
      
      cat("    ", rmse_pic,  "\n", file=out_file_log, sep="", append=T)
      
    }else{
      cat("    NO INPUT FILES FOUND", "\n", file=out_file_log, append=T, sep="")
      cat("    WARN: NO OUTPUT PLOTS GENERATED\n", sep="", file=out_file_log, append=T)
    }
    
  }
  
}

