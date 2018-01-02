#!/usr/bin/env Rscript

# Program in R - Month-by month evolution of interference incidence
# P. Altube Vazquez - July 2017

#########################################################################################
##### INFO and USAGE ####################################################################



#########################################################################################
##### LIBRARIES #########################################################################

library(plyr, quietly=T)
library(ggplot2, quietly=T)

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

#########################################################################################
## SETTINGS #############################################################################

# minimum incidence threshold considered in interference clustering
th_percent <- 2
# Maximum distance between interferences (deg) to be considered as coming from same emitter
dist_max <- 6

#########################################################################################
## DEFAULT SETTINGS #####################################################################

work_path <- "/home/operator/progR/SunINTFCal"
# work_path <- "/home/pav/Desktop/WIFIopera"

# Start and end dates of the analysis
date_str_i <- "170201"
date_str_f <- "170731"

radar <- "CDV"

# Boolean that indicates whether the date info is included in the name of output figures
date_bool <- T

#########################################################################################
## PARAMETERS ###########################################################################

# Customised colorbar
colbreaks <- c(0, 0.5, 1, 2, 5, 10, 20, 50, 100)
colscale <- c("white", "ivory2", "lightgoldenrodyellow", "lightgoldenrod1", "gold2", "darkorange2", "orangered3", "black")

#########################################################################################
## ARGUMENTS ############################################################################

# Retrieve command-line argument(s): Date and radar name
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

date_str_i <- paste(format(date_obj_i, format="%y%m"), "01", sep="")
date_str_f <- paste(format(date_obj_f, format="%y%m"), "01", sep="")

date_obj_i <- as.Date(date_str_i, format="%y%m%d")
date_obj_f <- as.Date(date_str_f, format="%y%m%d")

date_obj_seq <- seq(date_obj_i, date_obj_f, 1)
ym_str_seq <- unique(format(date_obj_seq, format="%y%m"))

ym_lims <- format(c(min(date_obj_seq), max(date_obj_seq)), format="%b%Y")

# LOG file:
log_path <- paste(work_path, radar, "LOG/", sep="/")
out_file_log <- paste(log_path, "LOGsunINTF_", date_str_f, "_", radar, ".txt", sep="")

# Input and Output paths
in_path <- paste(work_path, radar, "Emitter", sep="/")
out_path <- paste(work_path, radar, "Emitter", sep="/")

## OUTPUT ###########################################################################

if (!file.exists(out_path)){
  dir.create(out_path, recursive=T)
}

# Output figure
if (date_bool){
  out_fig <- paste(out_path, "/INTFts_", ym_lims[1], "_", ym_lims[2], "_", radar, ".png", sep="")
}else{
  out_fig <- paste(out_path, "/INTFts_", radar, ".png", sep="")
}

#########################################################################################
## CORE #################################################################################

# LOG file: execution message
cat("\n# EXECUTION: INTFcluster.R\n", "  ", as.character(time_now), "\n",
    file=out_file_log, append=T, sep="")
cat("\n# EXECUTION: INTFcluster.R ", as.character(time_now), "\n", sep="")
cat(out_file_log)
## INPUT DATA ###########################################################################

cat("    INPUT:\n", file=out_file_log, sep="", append=T)

in_files <- c()
for (i in seq(1, length(ym_str_seq))){
  
  ym <- ym_str_seq[i]
  in_files <- c(in_files, paste(in_path, "/INTFhist_", ym, "_", radar, ".txt", sep=""))
  
}

ava_files <- in_files[file.exists(in_files)]
ava_ym_str <- ym_str_seq[file.exists(in_files)]
ava_dates_obj <- date_obj_seq[file.exists(in_files)]

cat(length(ava_files))
if (length(ava_files)!=0){
  
  data_hist <- data.frame()
  for (j in seq(1, length(ava_files))) {
    
    ym <- ava_ym_str[j]
    in_file <- ava_files[j]
    day <- as.Date(paste(ym, "01", sep=""), format="%y%m%d")
    
    cat("    ", in_file, "\n", file=out_file_log, sep="", append=T)
    
    data_hist_tmp <- read.table(in_file, header = T)
    data_hist_tmp <- subset(data_hist_tmp, select=c(az_m, percent))
    data_hist_tmp$date <- day
    
    data_hist_tmp <- data_hist_tmp[data_hist_tmp$percent>=th_percent,]
    
    data_hist <- rbind(data_hist, data_hist_tmp)
    
  }
  
   # Group closely located interferences (hierarchical clustering)
  daz_mat <- dist(sort(unique(data_hist$az_m[data_hist$percent!=0])), method = "euclidean") # distance matrix
  n_int <- length(daz_mat)
  
  if (n_int!=0){
    
    fit <- hclust(daz_mat, method="median")
    # Data frame (az_labs): indicates to which group belongs each affected azimuth
    az_labs <- data.frame(az=sort(unique(data_hist$az_m[data_hist$percent!=0])), gr=cutree(fit, h=dist_max))
    # Data frame (az_groups): statistics of each group
    az_groups <- ddply(az_labs, .(gr), summarise, az_min=min(az), az_max=max(az), 
                       az_med=median(az), az_mean=mean(az))
    
    # Label interferences based on the obtained clusters; add group labels and characteristics to data_hist dataframe
    data_hist$gr <- NA
    data_hist$az_gr <- NA
    data_hist$az_min <- NA
    data_hist$az_max <- NA
    
    # Data frame (poly_gr): indicates date and az limits of the groups for drawing group-indicator polygons
    poly_gr <- data.frame() 
    for (j in seq(1, nrow(az_groups))){
      
      gr <- az_groups$gr[j]
      az_min <- az_groups$az_min[j]
      az_max <- az_groups$az_max[j]
      
      data_hist$gr[(data_hist$az_m<=az_max)&(data_hist$az_m>=az_min)] <- gr
      data_hist$az_gr[(data_hist$az_m<=az_max)&(data_hist$az_m>=az_min)] <- az_groups$az_med[j]
      data_hist$az_min[(data_hist$az_m<=az_max)&(data_hist$az_m>=az_min)] <- az_min
      data_hist$az_max[(data_hist$az_m<=az_max)&(data_hist$az_m>=az_min)] <- az_max
      
      poly_gr_tmp <- data.frame(gr=rep(gr,4), x=c(date_obj_f, date_obj_i, date_obj_i, date_obj_f), 
                                y=c(az_min, az_min, az_max, az_max))
      poly_gr <- rbind(poly_gr, poly_gr_tmp)
      
    }
    
    # Join interferences based on the label/cluster they pertain to
    data_group <- ddply(data_hist,.(gr, date, az_gr, az_min, az_max), summarize, percent=sum(percent))
    data_group[data_group$percent<th_percent,] <- NA
    data_group <- data_group[complete.cases(data_group$percent),]
    poly_gr <- poly_gr[poly_gr$gr %in% data_group$gr,]
    
  }else{
    
    data_group <- data.frame(date=date_obj_seq, gr=rep(0, length(date_obj_seq)), az_gr=rep(0, length(date_obj_seq)),
                             az_min=rep(0, length(date_obj_seq)), az_max=rep(0, length(date_obj_seq)), 
                             percent=rep(0, length(date_obj_seq))) 
    
  }
  
  ##  PLOTTING  #######################################################################
  
  ym_i <- format(date_obj_i, format="%b %Y")
  ym_f <- format(date_obj_f, format="%b %Y")
  tit <- paste("External interferences:", radar, ym_i, "-", ym_f, sep=" ")
  
  year_i <- format(date_obj_i, format="%Y")
  year_f <- format(date_obj_f, format="%Y")
  if (year_f==year_i){
    x_tit <- year_i
  }else{
    x_tit <- paste(ym_i, "-", ym_f, sep="")
  }
  
  p <- ggplot(data = data_group, aes(x=date, y=az_gr)) +
    scale_x_date(date_breaks = "month", date_labels = "%b") +
    scale_y_continuous(breaks=seq(0, 360, 30), labels = plot_labels(seq(0, 360, 60), seq(0, 360, 30))) +
    coord_cartesian(ylim=c(-5, 365)) + theme_bw() +
    xlab(x_tit) + ylab("Azimuth from North [deg]") +
    ggtitle(tit) +
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
          legend.text=element_text(size=14),
          legend.key.size = unit(1, "cm"),
          legend.title = element_text(size=16))
  
  
  if (exists("poly_gr")){
    p <- p + geom_polygon(data=poly_gr, aes(x=x, y=y, group=gr), col="grey90", fill="grey80", alpha=0.5) +
      annotate("text", x=date_obj_f + 10, y=unique(data_group$az_gr), 
               label=paste(unique(data_group$az_min),"º", "-", unique(data_group$az_max), "º", sep=""), 
               colour="grey30", size=3.5)
  }
  
  if (n_int!=0){
    p <- p + geom_point(aes(fill=cut(percent, colbreaks)), size=4.5, alpha=0.8, colour="grey30", shape=21) + 
      scale_fill_manual(values = colscale, limits=levels(cut(data_group$percent, colbreaks)), name="Affected PPIs [%]")
  }
  
  cat("    OUTPUT:\n", file=out_file_log, sep="", append=T)
  cat("    ", out_fig, "\n", file=out_file_log, sep="", append=T)
  
  png(file=out_fig, width=1200, height=600)
  print(p)
  dev.off()
  
}else{
  
  cat("    No input files found for the selected time period\n", file=out_file_log, sep="", append=T)
  cat("    NO OUTPUT GENERATED\n", file=out_file_log, sep="", append=T)
  
}