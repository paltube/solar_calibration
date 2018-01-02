#!/bin/bash

## To be executed DAILY, after all the RAW files for the day have been received and stored ##
## Daily application of the online Sun monitoring method 
## (+redirection of graphical output to web folder for display)

# VARIABLES #############################################################################

# Set paths
ROOTpath="/home/operator/progR" # Modify in case of machine change 
WEBpath="/var/www/html/SunINTFCal" # Out path for web plots
HISTpath="/ARX/METADADES/SunINTFCal" # Data storage path
RAWpath="/ARX/IRIS/RAW" # Path to RAW .tgz storage folder

# Radar name array
declare -a RADARS=("CDV" "LMI" "PBE" "PDA")

# Path to Rscript command in operator@smcprotdt02
RS="/usr/bin/Rscript"

# DATE VARIABLES ########################################################################

# Find out previous day's (system) date

DATE=$(date --date="yesterday" +%y%m%d)
DATEst=$(date --date="2 months ago" +%y%m%d)

YY=$(date --date="yesterday" +%y)
MM=$(date --date="yesterday" +%m)
DD=$(date --date="yesterday" +%d)

YYlong=$(date --date="yesterday" +%Y) # Evita el "efecto 3000" :)

DATE_name=$YY$MM$DD

# RELATIVE VARIABLES ####################################################################

WORKpath="$ROOTpath/SunINTFCal" # Main path of the processes
LOGpath="$WORKpath/LOGsh" # Path to LOG file containing folder
TMPpath="$WORKpath/tmp" # Path to temporary folder containing RAW file
SCRIPTpath="$WORKpath/Scripts" # Path to R and shell scripts containing folder

# Initialization settings' file
INIfile=$WORKpath/SunINTFCal.ini

# LOG file for standard and error output redirection
LOGfileIN=$TMPpath/LOGsunINTF_sh.txt

# Date identified LOG file that records errors from shell script runs
LOGfileOUT=$LOGpath/LOGsunINTF_sh_$DATE.txt

#########################################################################################

mkdir -p $TMPpath # Create folder only if it does not exist already
mkdir -p $LOGpath # Create folder only if it does not exist already

exec &> $LOGfileIN

# Working directory:
cd $TMPpath

echo $DATE

# Run the scripts for each of the three operative radars
for r in "${RADARS[@]}"
do
	
	echo $r
	
	WORKpathPLOTS=$WORKpath/$r/Plots
	WORKpathPOLAR=$WORKpathPLOTS/Polar
	HISTpathPLOTS=$HISTpath/$r/$YYlong/$MM/Plots
	HISTpathSCATTER=$HISTpathPLOTS/Scatter
	HISTpathPOLAR=$HISTpathPLOTS/Polar
	WEBpathPLOTS=$WEBpath/$r
	WEBpathPOLAR=$WEBpath/$r/Polar
	

	DATApath="$RAWpath/$r/$YYlong/$MM"
	File=${r}RAW$YYlong$MM$DD.tgz

	echo $File
	echo $DATE $r

	# Copy and decompress .tgz file
	cp $DATApath/$File $TMPpath
	gunzip < $File | tar xf -

	# RUN SCRIPTS
	# Sun interference detection and characterization:
	$RS $SCRIPTpath/a_INTF_detector.R $DATE ${r} $File $WORKpath $INIfile
	$RS $SCRIPTpath/b_SunINTF_detector.R $DATE ${r} $WORKpath $INIfile
	# Model inversion (5 and 3 parameter fit)
	$RS $SCRIPTpath/c_LS_inversion.R $DATE ${r} $WORKpath $INIfile
	# Generation of calibration (inversion) output plots
	$RS $SCRIPTpath/d_TS_plots.R $DATEst $DATE ${r} $WORKpath NODATE
	# Generation of polar interference plots
	$RS $SCRIPTpath/e_POLAR_plots.R $DATE ${r} $WORKpath NODATE
	# Generation of number of interferences calendar
	$RS $SCRIPTpath/f_CALENDAR_plot.R $DATE ${r} $WORKpath NODATE
	
	# Store daily polar plots for historical record (add date info)
	mkdir -p $HISTpathPOLAR # Create folder only if it does not exist already

	for f in $(ls $WORKpathPOLAR/Polar*.png)
	do 	
		f_out=${f//$WORKpathPOLAR/$HISTpathPOLAR}
		cp $f ${f_out//_${r}.png/_${DATE_name}_${r}.png}
	done

	mkdir -p $HISTpathSCATTER # Create folder only if it does not exist already

	for f in $(ls $WORKpathPLOTS/Scatter*.png)
	do
		f_out=${f//$WORKpathPLOTS/$HISTpathSCATTER}
		cp $f ${f_out//_${r}.png/_${DATE_name}_${r}.png}
	done

	# Move all generated plots to web folder
	mkdir -p $WEBpathPOLAR # Create folder only if it does not exist already
	cp $WORKpathPOLAR/*.png $WEBpathPOLAR/
	cp $WORKpathPLOTS/*.png $WEBpathPLOTS/
	
	# Remove remaining RAW files (PPIVOL_A)
	rm $TMPpath/*.tgz
	rm $TMPpath/*.RAW*
	
done

# Remove R objects
rm $TMPpath/*.RData

# Move shell LOG file to LOG file containing folder
mv $LOGfileIN $LOGfileOUT


