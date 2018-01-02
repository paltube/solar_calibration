#!/usr/bin/env Rscript

#########################################################################################

# Program in R - Calendar plot of the daily number of detected solar interferences
# P. Altube Vazquez - July 2017

##### INFO and USAGE ####################################################################

# This script has been developed for its automatic and daily execution within the solar 
# monitoring project (SunINTFCal). The shell script that boots up the daily proccess is: 
# "SunINTFCal_daily.sh"
#                                                                                    
# This script loads information about the number of solar interferences daily detected 
# (input files: Calib_MP_Nday_YYMM_XXX.txt) and generates a calendar picture for the
# last year, daily colorcoded as a function of the number of detected interferences:
# "NINTFcalendar_YYMMDD_XXX.png"
#
# Command line execution: specify date (last day of year for which data is to be 
# plotted), radar abbrv. and workpath:                                   
# Rscript .../f_CALENDAR_plot.R YYMMDD XXX ...                         
# If no date information is required in the names of the output figures add "NODATE" as
# a last command line argument:                                       

#########################################################################################

options(warn=-1)

library(getopt, quietly=T)
library(lattice, quietly=T)
library(grid, quietly=T)
library(chron, quietly=T)
library(latticeExtra, quietly=T)

time_now <- Sys.time()

##  SETTINGS  ###########################################################################

# Number of header lines in the input file:
hdr_length <- 15

# Name of the pictures to be generated
out_pic_name <- "NINTFcalendar_"

##### DEFAULT SETTINGS ##################################################################

# Set date and radar abbrv. if not provided as command line arguments:
date_str_f <- "170717"
radar <- "CDV"

work_path <- "/home/operator/progR/SunINTFCal"

##### FUNCTIONS #########################################################################

# Function for plotting the calendar
calendar_plot <- function(dates, values, date.range=NULL, main="Values",
                               color="r2g", ncolors=99,
                               date.form = "%Y-%m-%d", cex=1) {
  if (class(dates) == "character" | class(dates) == "factor" ) {
    dates <- strptime(dates, date.form)
  }
  caldat <- data.frame(value = values, dates = dates)
  
  if (is.null(date.range)){
    min.date <- as.Date(paste(format(min(dates), "%Y"), "-1-1",sep = ""))
    max.date <- as.Date(paste(format(max(dates), "%Y"), "-12-31", sep = ""))
  }else{
    min.date <- as.Date(paste(format(date.range[1], "%Y"), "-1-1",sep = ""))
    max.date <- as.Date(paste(format(date.range[2], "%Y"), "-12-31",sep = ""))
  }
  dates.f <- data.frame(date.seq = seq(min.date, max.date, by="days"))
  daysP <- as.character(format(dates.f$date.seq, "%d"))
  
  caldat <- data.frame(date.seq = seq(min.date, max.date, by="days"), value = NA)
  dates <- as.Date(dates) 
  caldat$value[match(dates, caldat$date.seq)] <- values
  
  caldat$dotw <- as.numeric(format(caldat$date.seq, "%w"))
  caldat$woty <- as.numeric(format(caldat$date.seq, "%U")) + 1 #week of the year
  caldat$yr <- as.factor(format(caldat$date.seq, "%Y"))
  caldat$month <- as.numeric(format(caldat$date.seq, "%m"))
  yrs <- as.character(unique(caldat$yr))
  
  d.loc <- as.numeric()
  txt <<- list()
  xx <<- list()
  yy <<- list()
  count <- 1
  for (m in min(yrs):max(yrs)) {
    d.subset <- caldat[caldat$yr == m, ]
    sub.seq <- seq(1, nrow(d.subset))
    txt[[count]] <<- format(d.subset$date, format="%d")
    yy[[count]] <<- d.subset$dotw
    xx[[count]] <<- d.subset$woty
    d.loc <- c(d.loc, sub.seq)
    count <- count +1
  }
  caldat <- cbind(caldat, seq=d.loc)

  #color styles
  r2b <- c("#0571B0", "#92C5DE", "#F7F7F7", "#F4A582", "#CA0020") #red to blue                                                                               
  r2g <- c("#D61818", "#FFAE63", "#FFFFBD", "#B5E384")   #red to green
  w2b <- c("#045A8D", "#2B8CBE", "#74A9CF", "#BDC9E1", "#F1EEF6")   #white to blue
  heat <- rev(heat.colors(30))
  
  assign("col.sty", get(color))
  calendar.pal <- colorRampPalette((col.sty), space = "Lab")
  
  #   calendar.pal <- c("olivedrab3", "khaki1", "darkorchid1", "lightblue1", "salmon") 
  colseq <- c(seq(0,100, 100/ncolors),1000)
  def.theme <- lattice.getOption("default.theme")
  cal.theme <-
    function() {  
      theme <-
        list(
          strip.background = list(col = "transparent"),
          strip.border = list(col = "transparent"),
          axis.line = list(col="transparent"),
          par.strip.text=list(cex=cex))
    }
  lattice.options(default.theme = cal.theme)
  yrs <- (unique(caldat$yr))
  nyr <- length(yrs)

 print(cal.plot <- levelplot(value~woty*dotw|yr, data=caldat,
                              as.table=TRUE, aspect=.2,
                              layout = c(1, nyr%%7),
                              between = list(x=0, y=c(1,1)),
                              strip=TRUE, main = list(label=main, cex=1.5*cex),
                              at = colseq,
                              scales = list(x = list(at= c(seq(2.9, 52, by=4.42)), labels = month.abb,
                                                     alternating = c(1, rep(0, (nyr-1))), tck=0, cex = cex),
                                            y=list(at = c(0, 1, 2, 3, 4, 5, 6), labels = c("Su", "Mo", "Tu", "We", "Th", "Fr", "Sa"),
                                            alternating = 1, cex = 0.8*cex, tck=0)),
                              xlim =c(0.4, 54.6),
                              ylim=c(6.6,-0.6),
                              col.regions = c(calendar.pal(ncolors),"#666666"),
                              xlab="" ,
                              ylab="",
                              colorkey= list(col = c(calendar.pal(ncolors),"#666666"),
                                             at=seq(0,100+100/ncolors, 100/ncolors),
                                             labels = list(at =seq(50/ncolors, 100+50/ncolors, 100/ncolors), cex=cex,
                                                           lab = c(seq(50/ncolors, 100, 100/ncolors), ">100"), width = 10, height = 10)),
                              subscripts=TRUE) + 
         layer(panel.text(xx[[panel.number()]], yy[[panel.number()]], txt[[panel.number()]], cex=0.4)))

  panel.locs <- trellis.currentLayout()
  
  for (row in 1:nrow(panel.locs)) {
    
    for (column in 1:ncol(panel.locs))  {
      
      if (panel.locs[row, column] > 0){
        
        trellis.focus("panel", row = row, column = column, highlight = FALSE)
        
        xyetc <- trellis.panelArgs()
        subs <- caldat[xyetc$subscripts,]
        dates.fsubs <- caldat[caldat$yr == unique(subs$yr),]
        y.start <- dates.fsubs$dotw[1]
        y.end   <- dates.fsubs$dotw[nrow(dates.fsubs)]
        dates.len <- nrow(dates.fsubs)
        adj.start <- dates.fsubs$woty[1]
        
        for (k in 0:6) {
          if (k < y.start) {
            x.start <- adj.start + 0.5
          } else {
            x.start <- adj.start - 0.5
          }
          if (k > y.end) {
            x.finis <- dates.fsubs$woty[nrow(dates.fsubs)] - 0.5
          } else {
            x.finis <- dates.fsubs$woty[nrow(dates.fsubs)] + 0.5
          }
          grid.lines(x = c(x.start, x.finis), y = c(k -0.5, k - 0.5), 
                     default.units = "native", gp=gpar(col = "grey", lwd = 1))
        }
        if (adj.start <  2) {
          grid.lines(x = c( 0.5,  0.5), y = c(6.5, y.start-0.5), 
                     default.units = "native", gp=gpar(col = "grey", lwd = 1))
          grid.lines(x = c(1.5, 1.5), y = c(6.5, -0.5), default.units = "native",
                     gp=gpar(col = "grey", lwd = 1))
          grid.lines(x = c(x.finis, x.finis), 
                     y = c(dates.fsubs$dotw[dates.len] -0.5, -0.5), default.units = "native",
                     gp=gpar(col = "grey", lwd = 1))
          if (dates.fsubs$dotw[dates.len] != 6) {
            grid.lines(x = c(x.finis + 1, x.finis + 1), 
                       y = c(dates.fsubs$dotw[dates.len] -0.5, -0.5), default.units = "native",
                       gp=gpar(col = "grey", lwd = 1))
          }
          grid.lines(x = c(x.finis, x.finis), 
                     y = c(dates.fsubs$dotw[dates.len] -0.5, -0.5), default.units = "native",
                     gp=gpar(col = "grey", lwd = 1))
        }
        for (n in 1:51) {
          grid.lines(x = c(n + 1.5, n + 1.5), 
                     y = c(-0.5, 6.5), default.units = "native", gp=gpar(col = "grey", lwd = 1))
        }
        x.start <- adj.start - 0.5
        
        if (y.start > 0) {
          grid.lines(x = c(x.start, x.start + 1),
                     y = c(y.start - 0.5, y.start -  0.5), default.units = "native",
                     gp=gpar(col = "black", lwd = 1.75))
          grid.lines(x = c(x.start + 1, x.start + 1),
                     y = c(y.start - 0.5 , -0.5), default.units = "native",
                     gp=gpar(col = "black", lwd = 1.75))
          grid.lines(x = c(x.start, x.start),
                     y = c(y.start - 0.5, 6.5), default.units = "native",
                     gp=gpar(col = "black", lwd = 1.75))
          if (y.end < 6  ) {
            grid.lines(x = c(x.start + 1, x.finis + 1),
                       y = c(-0.5, -0.5), default.units = "native",
                       gp=gpar(col = "black", lwd = 1.75))
            grid.lines(x = c(x.start, x.finis),
                       y = c(6.5, 6.5), default.units = "native",
                       gp=gpar(col = "black", lwd = 1.75))
          } else {
            grid.lines(x = c(x.start + 1, x.finis),
                       y = c(-0.5, -0.5), default.units = "native",
                       gp=gpar(col = "black", lwd = 1.75))
            grid.lines(x = c(x.start, x.finis),
                       y = c(6.5, 6.5), default.units = "native",
                       gp=gpar(col = "black", lwd = 1.75))
          }
        } else {
          grid.lines(x = c(x.start, x.start),
                     y = c( - 0.5, 6.5), default.units = "native",
                     gp=gpar(col = "black", lwd = 1.75))
        }
        
        if (y.start == 0 ) {
          if (y.end < 6  ) {
            grid.lines(x = c(x.start, x.finis + 1),
                       y = c(-0.5, -0.5), default.units = "native",
                       gp=gpar(col = "black", lwd = 1.75))
            grid.lines(x = c(x.start, x.finis),
                       y = c(6.5, 6.5), default.units = "native",
                       gp=gpar(col = "black", lwd = 1.75))
          } else {
            grid.lines(x = c(x.start + 1, x.finis),
                       y = c(-0.5, -0.5), default.units = "native",
                       gp=gpar(col = "black", lwd = 1.75))
            grid.lines(x = c(x.start, x.finis),
                       y = c(6.5, 6.5), default.units = "native",
                       gp=gpar(col = "black", lwd = 1.75))
          }
        }
        for (j in 1:12)  {
          last.month <- max(dates.fsubs$seq[dates.fsubs$month == j])
          x.last.m <- dates.fsubs$woty[last.month] + 0.5
          y.last.m <- dates.fsubs$dotw[last.month] + 0.5
          grid.lines(x = c(x.last.m, x.last.m), y = c(-0.5, y.last.m),
                     default.units = "native", gp=gpar(col = "black", lwd = 1.75))
          if ((y.last.m) < 6) {
            grid.lines(x = c(x.last.m, x.last.m - 1), y = c(y.last.m, y.last.m),
                       default.units = "native", gp=gpar(col = "black", lwd = 1.75))
            grid.lines(x = c(x.last.m - 1, x.last.m - 1), y = c(y.last.m, 6.5),
                       default.units = "native", gp=gpar(col = "black", lwd = 1.75))
          } else {
            grid.lines(x = c(x.last.m, x.last.m), y = c(- 0.5, 6.5),
                       default.units = "native", gp=gpar(col = "black", lwd = 1.75))
          }
        }
      }
      
    }
    trellis.unfocus()
  } 
  lattice.options(default.theme = def.theme)
}

##### VARIABLES #########################################################################

#Retrieve command-line argument(s): Date and radar name
options <- commandArgs(trailingOnly=T)

if (!is.na(options[1])){
  date_str_f <- options[1]
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
  date_bool <- T
}

# Start date is 1 year before
date_obj_f <- as.Date(date_str_f, format="%y%m%d")
date_obj_i <- seq(date_obj_f, length=2, by="-1 years")[2]

date_str_i <- format(date_obj_i, format="%y%m%d")
date_tit_i <- format(date_obj_i, format="%d %b %Y")
date_tit_f <- format(date_obj_f, format="%d %b %Y")

# Sequence of dates delimited by the input-date pair:
date_obj_seq <- seq(date_obj_i, date_obj_f, 1)
# Collection of Year-Month pairs within the sequence:
ym_seq <- unique(format(date_obj_seq, format="%y%m"))

# LOG file:
log_path <- paste(work_path, radar, "LOG", sep="/")
out_file_log <- paste(log_path, "/LOGsunINTF_", date_str_f, "_", radar, ".txt", sep="")

cat("\n", "# EXECUTION: f_CALENDAR_plot.R\n", "  ", as.character(time_now), "\n",
    file=out_file_log, append=T, sep="")
cat("\n", "# EXECUTION: f_CALENDAR_plot.R ", as.character(time_now), "\n", sep="")

in_path <- paste(work_path, radar, sep="/")
#Output picture:
out_path <- paste(work_path, radar, "Plots/", sep="/")
if (!file.exists(out_path)){dir.create(out_path, showWarnings =F, recursive = T)}

if (date_bool){
  out_pic <- paste(out_path, out_pic_name, date_str_f, "_", radar, ".png", sep="")
}else{
  out_pic <- paste(out_path, out_pic_name, radar, ".png", sep="")
}

##### DATA LOADING ######################################################################

data_all <- data.frame()

cat("  # Required input files:\n", file=out_file_log, sep="", append=T)

# Data loading loop for all year-month pairs within the selected time span:
for (i in seq(1, length(ym_seq))){
  
  ym <- ym_seq[i]
  
  in_file <- paste(in_path, "/Calib_5P_1day_", ym, "_", radar, ".txt", sep="")
  cat("    ", in_file, file=out_file_log, sep="", append=T)
  
  # Reading input files
  if (file.exists(in_file)){
    
    data_tmp <- read.table(file=in_file, sep= " ", header=T, skip=hdr_length)
    
    if (nrow(data_tmp!=0)){
      
      cat("    FILE OK\n", file=out_file_log, append=T)
      data_tmp$date <- as.Date(data_tmp$date)
      data_tmp <- subset(data_tmp, select=c(date, n_intf))
      
      # Collect data from all input files in a single dataframe
      data_all <- rbind(data_all, data_tmp)
      
    }else{
      cat("    No data in input file\n", file=out_file_log, append=T)
      
    }

  }else{
    cat("    Input file not found\n", file=out_file_log, append=T)
  }
      
}

data_all <- data_all[data_all$date>=date_obj_i,]
data_all <- data_all[data_all$date<=date_obj_f,]

# Order dataframe rows by date
data_all <- data_all[order(data_all[,"date"]), ]
data_all <- data_all[!is.na(data_all$date), ]

cat("  # Creating calendar ", date_str_f, " - ", date_str_i,  " :    ", file=out_file_log, sep="", append=T)
cat(out_pic,"\n", file=out_file_log, sep="", append=T)

png(file=out_pic, bg="transparent", height=1000, width=1500, res=200)
calendar_plot(data_all$date, data_all$n_intf, date.range=c(date_obj_i, date_obj_f), ncolors=10,
                   main=paste(radar, ", ", date_tit_i, "-", date_tit_f, sep=""), 
                   color="heat", cex=0.8)
dev.off()

cat("\n", sep="", file=out_file_log, append=T)

#########################################################################################
## EOS ##
#########################################################################################
