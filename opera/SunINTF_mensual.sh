#!/bin/bash

## To be executed on the FIRST DAY of each MONTH, after execution of the daily shell script ##
## Monthly storage of Sun Interference calibration results 
## (intf. records, calibration files, LOG files and output time-series plots)


# VARIABLES #############################################################################

# Set paths
ROOTpath="/home/operator/progR" # Modify in case of machine change 
WEBpath="/var/www/html/SunINTFCal" # Out path for web plots
HISTpath="/ARX/METADADES/SunINTFCal" # Data storage path

# Radar name array
declare -a RADARS=("CDV" "LMI" "PBE" "PDA")

# Path to Rscript command in operator@smcprotdt02
RS="/usr/bin/Rscript"

# RELATIVE VARIABLES ####################################################################

WORKpath="$ROOTpath/SunINTFCal" # Main path of the processes
SCRIPTpath="$WORKpath/Scripts"

# DATE VARIABLES ########################################################################

DATE=$(date --date="yesterday" +%y%m%d)
DATE_1d=$(date --date="2 days ago" +%y%m%d)
DATE_2d=$(date --date="3 days ago" +%y%m%d)

DATEst_1m=$(date --date="1 months ago" +%y%m%d)
DATEst_6m=$(date --date="6 months ago" +%y%m%d)
DATErm_7m=$(date --date="7 months ago" +%y%m)

YY=$(date --date="yesterday" +%y)
MM=$(date --date="yesterday" +%m)
YYlong=$(date --date="yesterday" +%Y)

DATE_name=$YY$MM

#########################################################################################

for r in "${RADARS[@]}"
do

	CALIBpath=$WORKpath/$r
	OUTpath=$HISTpath/$r/$YYlong/$MM

	WORKpathPLOTS=$CALIBpath/Plots
	HISTpathPLOTS=$OUTpath/Plots
	WEBpathPLOTS=$WEBpath/$r
	
	OUTpathEMIT=$CALIBpath/Emitter
	HISTpathEMIT=$OUTpath/Emitter
	WEBpathEMIT=$WEBpathPLOTS/Emitter
	
	RECpath=$CALIBpath/Rec
	LOGpath=$CALIBpath/LOG
	
	# RLAN interference incidence
        $RS $SCRIPTpath/INTFopera.R $DATEst_1m $DATE ${r} $WORKpath NODATE
	
	$RS $SCRIPTpath/INTFcluster.R $DATEst_6m $DATE ${r} $WORKpath NODATE
	
        mkdir -p $HISTpathEMIT
	mkdir -p $WEBpathEMIT

        # Store data files in historic path
	cp $OUTpathEMIT/INTF*.txt $HISTpathEMIT
        mv $OUTpathEMIT/INTF*.tif $HISTpathEMIT	
        
	# Copy figure to web path
	cp $OUTpathEMIT/INTF*.png $WEBpathEMIT	
	
	# Store files in historic path
        for f in $(ls $OUTpathEMIT/INTF*.png)
	do
		f_out=${f//$OUTpathEMIT/$HISTpathEMIT}
		cp $f ${f_out//_${r}./_${DATE_name}_${r}.}
	done
	
	rm $OUTpathEMIT/INTF*$DATErm_7m*
            
	# STORAGE in HIST path
	TGZintf=$OUTpath"/INTFfiles_"$YY$MM"_"$r".tgz"
	TGZlogs=$OUTpath"/LOGfiles_"$YY$MM"_"$r".tgz"
				
	# INTERFERENCE RECORDING FILES
	cd $RECpath
	# Compress
	tar -czf $TGZintf *INTF*$YY$MM*$r.txt

	# Delete compressed files except the last three days'
	find `pwd` -maxdepth 1 \( -name "*INTF*" -and -not -name "*INTF_${DATE_2d}_${r}.txt" -and -not -name "*INTF_${DATE_1d}_${r}.txt" -and -not -name "*INTF_${DATE}_${r}.txt" \) | xargs -i rm {}

	# CALIBRATION FILES (Only deleted at the end of the year)
	cp $CALIBpath/Calib*$YY$MM*.txt $OUTpath 
	
	# LOG FILES
	cd $LOGpath
	# Compress
	tar -czf $TGZlogs *LOG*$YY$MM*$r.txt
	echo $TGZlogs
	rm *LOG*$YY$MM*$r.txt

	# MONTHLY CALIBRATION PLOTS (copy all except current day scatter plots)
	cd $WEBpathPLOTS

	for f in $(find `pwd` -not -path $WEBpathPLOTS -not \( -path $WEBpathEMIT -prune \) -not \( -path $WEBpathPLOTS/Polar -prune \) \( -not -name "Scatter*" \))
	do 	
		ff=${f//$WEBpathPLOTS/$HISTpathPLOTS}
		cp $f ${ff//_${r}.png/_${DATE_name}_${r}.png}
	done

done

