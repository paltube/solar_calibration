#!/usr/bin/env Rscript

# Program in R - Polar plots of detected interferences (frequency and intensity)
# P. Altube Vazquez - July 2017

##### INFO and USAGE ####################################################################

# This script has been developed for its automatic and daily execution within the solar 
# monitoring project (SunINTFCal). The shell script that boots up the daily proccess is: 
# "SunINTFCal_daily.sh"
#                                                                                    
# This script loads the detected interference data (input files: "INTF_YYMMDD_XXX.txt" 
# and "SunINTF_YYMMDD_XXX.txt") and generates graphical output in polar format.
# PolarFREQ_ALL_YYMMDD_XXX.png ; PolarFREQ_SOLAR_YYMMDD_XXX.png
# PolarMEAN_ALL_YYMMDD_XXX.png ; PolarMEAN_SOLAR_YYMMDD_XXX.png
# PolarMAX_ALL_YYMMDD_XXX.png ; PolarMAX_SOLAR_YYMMDD_XXX.png
# 
# Command line execution: specify date, radar abbrv. and workpath:
# Rscript .../e_POLAR_plots.R YYMMDD XXX ...              
# If no date information is required in the names of the output figures add "NODATE" as
# a last command line argument:  
# Rscript .../e_POLAR_plots.R YYMMDD XXX ... NODATE

#########################################################################################

options(warn=-1)

library(getopt, quietly=T)
library(plotrix, quietly=T)
library(ggplot2, quietly=T)
library(plyr, quietly=T)
library(RColorBrewer, quietly=T)

time_now <- Sys.time()

#########################################################################################
##### DEFAULT SETTINGS ##################################################################

# Date and radar abbrv. if not provided as command line arguments:
date_str <- "170802"
radar <- "PDA"

work_path <- "/home/operator/progR/SunINTFCal"
# work_path <- "/home/pav/Desktop/SUNtest"

#########################################################################################
##  SETTINGS  ###########################################################################

# Number of header lines in the input files:
hdr_length <- 15

# Name of the pictures to be generated
pic_freq_name <- "PolarFREQ"
pic_mean_name <- "PolarMEANpow"
pic_max_name <- "PolarMAXpow"

# Ranges of the polar plot (approximate)
range_min <- list("CDV"=2, "PDA"=2, "LMI"=2, "PBE"=2)
range_max <- list("CDV"=150, "PDA"=130, "LMI"=130, "PBE"=130)

# Azimuths:
brks_az <- seq(0.5, 359.5)
groups <- seq(1, length(brks_az)-1)

# Colorbar breaks (discrete)
# brks_pow <- seq(-118, -107)
brks_pow <- c(-Inf, seq(-117, -108), Inf)

# Colorbar labels
labs_pow <- c(paste("<", as.character(brks_pow)), "")

# Extended colorbar
cols_pow <- rev(c("grey75", colorRampPalette(c("blue", "yellow", "red"))(length(brks_pow)-3), "black"))

# Colorbar breaks (discrete)
brks_freq <- c(0.0001, 1, seq(2, 10, 2), 30, 50, Inf)
# Colorbar labels
labs_freq <- c(paste("<", as.character(brks_freq)), "")

# Extended colorbar
cols_freq <- rev(c("grey75", "ivory2", "lightgoldenrodyellow", 
                   "lightgoldenrod1", "gold2", "darkorange2", 
                   "orangered3", "darkred", "black"))

# Azimuthal and radial limits and labels for the polar plots
az_pos <- seq(0, 340, 20)*pi/180
az_labs <- as.character(seq(0,340,20))
rad_pos <- c(seq(0, range_max[[radar]], 50),  range_max[[radar]])
rad_labs <- as.character(rad_pos)
rad_labs <- c("", rad_labs[-1])
rad_labs <- c(rad_labs[-length(rad_labs)], "")

#########################################################################################
## PARAMETERS ###########################################################################

# Plot/interference types
types <- list("ALL"="", "SOLAR"="Sun")

#########################################################################################
##### ARGUMENTS #########################################################################

#Retrieve command-line argument(s): Date and radar name
options <- commandArgs(trailingOnly=T)

if (!is.na(options[1])){
  date_str <- options[1]
}
if (!is.na(options[2])){
  radar <- options[2]
}
if (!is.na(options[3])){
  work_path <- options[3]
}
if (!is.na(options[4]) & options[4]=="NODATE"){
  date_bool <- F
}else{
  date_bool <- F
}

#########################################################################################
## VARIABLES ############################################################################

warn_txt <- NA

# Date formatting
date_obj <- as.Date(date_str, format="%y%m%d")

# LOG file:
log_path <- paste(work_path, radar, "LOG/", sep="/")
out_file_log <- paste(log_path, "LOGsunINTF_", date_str, "_", radar, ".txt", sep="")
if (!file.exists(log_path)){dir.create(log_path, showWarnings =F, recursive = T)}

cat("\n# EXECUTION: e_POLAR_plots.R\n", "  ", as.character(time_now), "\n",
    file=out_file_log, append=T, sep="")
cat("\n# EXECUTION: e_POLAR_plots.R\n ", as.character(time_now), "\n", sep="")

# Input path
in_path <- paste(work_path, radar, "Rec", sep="/")

# Output path
out_path <- paste(work_path, radar, "Plots/Polar", sep="/")
if (!file.exists(out_path)){dir.create(out_path, showWarnings =F, recursive = T)}

# Define names for output plots with or without date info
date_name <- ""
if (date_bool){
  date_name <- paste(date_str, "_", sep="")
}

#########################################################################################
#####  SCRIPT CORE  #####################################################################
#########################################################################################

for (tt in names(types)){
  
  in_file <- paste(in_path, "/", types[[tt]], "INTF_", date_str, "_", radar, ".txt", sep="")
  
  cat("  # Input file: ", in_file, file=out_file_log, sep="", append=T)
  
  pic_freq <- paste(out_path, "/", pic_freq_name, "_", tt, "_", date_name, radar, ".png", sep="")
  pic_mean <- paste(out_path, "/", pic_mean_name, "_", tt, "_", date_name, radar, ".png", sep="")
  pic_max <- paste(out_path, "/", pic_max_name, "_", tt, "_", date_name, radar, ".png", sep="")
  
  tit_freq <- paste(radar, " ", date_obj, ", Frequency - ", tt, " intfs.", sep="")
  tit_mean <- paste(radar, " ", date_obj, ", Mean power - ", tt, " intfs.", sep="")
  tit_max <- paste(radar, " ", date_obj, ", Max. power - ", tt, " intfs.", sep="")
  
  if (file.exists(in_file)){
    
    line_num <- length(readLines(in_file))
    
    if (line_num > (hdr_length+1)){
      
      cat("    FILE OK", "\n", file=out_file_log, sep="", append=T)
      cat("    ", tt, " interferences:", "\n", file=out_file_log, sep="", append=T)
      
      data_intf <- read.table(file = in_file, skip = hdr_length, header = T)
      
      # Exclude outliers (keep only solar labelled interferences) and 
      # non-constant interferences (keep only constant labelled interferences)
      data_intf <- data_intf[(data_intf$label=="solar")|(data_intf$label=="constant"),]
      
      n_intf <- nrow(data_intf)
      
      # Group interferences by azimuth
      data_intf$gr <- NA
      az_i_0 <- brks_az[length(brks_az)]
      az_f_0 <- 0
      data_intf$gr[(data_intf$az_r>az_i_0)&(data_intf$az_r<=az_f_0)] <- 0
      
      for (i in groups){
        az_i <- brks_az[i]
        az_f <- brks_az[i+1]
        data_intf$gr[(data_intf$az_r>az_i)&(data_intf$az_r<=az_f)] <- i
      }
      
      # Derive statistics by azimuth
      data_groups <- ddply(data_intf, .(gr), summarise, mean_pow=median(pow), max_pow=max(pow), counts=length(gr))
      data_groups$val <- 10
    
    }else{
      
      warn_txt <- "NO INTERFERENCES FOUND"
      cat("    NO DATA IN FILE", "\n", file=out_file_log, append=T)
      
      data_groups <- data.frame(gr=groups, mean_pow=rep(0, length(groups)), max_pow=rep(0, length(groups)), 
                                counts=rep(0, length(groups)), val=rep(0, length(groups)))
    }
    
    levs_freq <- levels(cut(data_groups$counts, brks_freq))
    levs_freq <- c("(0,1]", levs_freq[-1])
    
    p_freq <- ggplot(data=data_groups, aes(x=gr, y=val, fill=cut(counts, brks_freq, na.rm=TRUE), colour=cut(counts, brks_freq))) +
      geom_bar(width = 1, stat = "identity") + 
      coord_polar(theta="x")+
      ggtitle(tit_freq) + 
      scale_x_continuous("", limits = c(0, 360), breaks = seq(0, 330, 30), labels = seq(0, 330, 30)) +
      scale_y_discrete(limits=c(0,10), breaks=NULL, labels="") +
      scale_fill_manual(values = rev(cols_freq), limits=levels(cut(data_groups$counts, brks_freq)), 
                        labels= levs_freq, name="Intf.\ncounts") +
      scale_color_manual(values = rev(cols_freq), limits=levels(cut(data_groups$counts, brks_freq)), 
                         labels= levs_freq, name="Intf.\ncounts") +
      theme(axis.line = element_blank(),
            axis.title.y=element_blank(),
            axis.text.x=element_text(size=22, colour="black"),
            panel.background = element_rect(fill = "transparent", colour = NA),
            panel.grid.major = element_line(size=0.5, colour="grey40", linetype = 1),
            panel.grid.minor = element_line(size=0.5, colour="grey70", linetype = 2),
            plot.title = element_text(lineheight=.3, face="bold", hjust = 0.5, size=26),
            plot.background = element_rect(fill = "transparent", colour = NA),
            legend.background = element_rect(fill = "transparent", colour = NA), 
            legend.text=element_text(size=17),
            legend.key.size = unit(1.5, "cm"),
            legend.title = element_text(size=20))
    
    p_mean <- ggplot(data=data_groups, aes(x=gr, y=val, fill=cut(mean_pow, brks_pow), colour=cut(mean_pow, brks_pow))) +
      geom_bar(width = 1, stat = "identity") + 
      coord_polar(theta="x")+
      ggtitle(tit_mean) + 
      scale_x_continuous("", limits = c(0, 360), breaks = seq(0, 330, 30), labels = seq(0, 330, 30)) +
      scale_y_discrete(limits=c(0,10), breaks=NULL, labels="") +
      scale_fill_manual(values = rev(cols_pow), limits=levels(cut(data_groups$mean_pow, brks_pow)), name="Mean Pow.\n[dBm]") +
      scale_color_manual(values = rev(cols_pow), limits=levels(cut(data_groups$mean_pow, brks_pow)), name="Mean Pow.\n[dBm]") +
      theme(axis.line = element_blank(),
            axis.title.y=element_blank(),
            axis.text.x=element_text(size=22, colour="black"),
            panel.background = element_rect(fill = "transparent", colour = NA),
            panel.grid.major = element_line(size=0.5, colour="grey40", linetype = 1),
            panel.grid.minor = element_line(size=0.5, colour="grey70", linetype = 2),
            plot.title = element_text(lineheight=.3, face="bold", hjust = 0.5, size=26),
            plot.background = element_rect(fill = "transparent", colour = NA),
            legend.background = element_rect(fill = "transparent", colour = NA), 
            legend.text=element_text(size=17),
            legend.key.size = unit(1.5, "cm"),
            legend.title = element_text(size=20))
    
    p_max <- ggplot(data=data_groups, aes(x=gr, y=val, fill=cut(max_pow, brks_pow), colour=cut(max_pow, brks_pow))) +
      geom_bar(width = 1, stat = "identity") + 
      coord_polar(theta="x")+
      ggtitle(tit_max) + 
      scale_x_continuous("", limits = c(0, 360), breaks = seq(0, 330, 30), labels = seq(0, 330, 30)) +
      scale_y_discrete(limits=c(0,10), breaks=NULL, labels="") +
      scale_fill_manual(values = rev(cols_pow), limits=levels(cut(data_groups$max_pow, brks_pow)), name="Max. Pow.\n[dBm]") +
      scale_color_manual(values = rev(cols_pow), limits=levels(cut(data_groups$max_pow, brks_pow)), name="Max. Pow.\n[dBm]") +
      theme(axis.line = element_blank(),
            axis.title.y=element_blank(),
            axis.text.x=element_text(size=22, colour="black"),
            panel.background = element_rect(fill = "transparent", colour = NA),
            panel.grid.major = element_line(size=0.5, colour="grey40", linetype = 1),
            panel.grid.minor = element_line(size=0.5, colour="grey70", linetype = 2),
            plot.title = element_text(lineheight=.3, face="bold", hjust = 0.5, size=26),
            plot.background = element_rect(fill = "transparent", colour = NA),
            legend.background = element_rect(fill = "transparent", colour = NA), 
            legend.text=element_text(size=17),
            legend.key.size = unit(1.5, "cm"),
            legend.title = element_text(size=20))
    

    if (!is.na(warn_txt)){
      p_freq <- p_freq + annotate("text", x=90, y=-5, label=warn_txt, size=16, colour="grey40")
      p_mean <- p_mean + annotate("text", x=90, y=-5, label=warn_txt, size=16, colour="grey40")
      p_max <- p_max + annotate("text", x=90, y=-5, label=warn_txt, size=16, colour="grey40")
    }
    
    cat("      Frequency polar plot: ", pic_freq, "\n", file=out_file_log, sep="", append=T)
    
    png(file=pic_freq, bg="transparent", height=1000, width=1000)
    print(p_freq)
    garbage <- dev.off()
    
    cat("      Mean power polar plot: ", pic_mean, "\n", file=out_file_log, sep="", append=T)
    
    png(file=pic_mean, bg="transparent", height=1000, width=1000)
    print(p_mean)
    garbage <- dev.off()
    
    cat("      Maximum power polar plot: ", pic_max, "\n", file=out_file_log, sep="", append=T)
    
    png(file=pic_max, bg="transparent", height=1000, width=1000)
    print(p_max)
    garbage <- dev.off()
    
  }else{
    cat("    FILE NOT FOUND", "\n", file=out_file_log, append=T)
    cat("    WARN: NO OUTPUT PLOTS GENERATED\n", sep="", file=out_file_log, append=T)
    
  }
  
}

cat("\n", file=out_file_log, append=T)

# #########################################################################################
# ## EOS ##
# #########################################################################################
