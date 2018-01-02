#!/bin/bash

echo "Content-type: text/html"
echo ""

IFS='&' # Change default whitespace splitting character for an ampersand
plots=()
COUNTER=1

# Retrieve query string variables and their values from html form submission
for item in $QUERY_STRING
do
	var=${item%%=*}
	val=${item#*=}
	
	# Multichoice checkbox variables
	if [ $(echo "$var" | grep -c "type") -ne 0 ]
	then
		plots[$COUNTER]="$val"
		COUNTER=`expr $COUNTER + 1`
	fi
	
	eval "$var=\$val"
	
done


# Out path for web plots with respect to "/var/www/html"
WEBpathPLOTS="/SunINTFCal/$radar"
WEBpathPOLAR="$WEBpathPLOTS/Polar"

# Write html
echo "<HTML>"
echo "<HEAD>"
echo "<title>SUN MONITORING</title>"
echo "</HEAD>"
echo "<BODY bgcolor="white">"
echo "<a name="TOP"></a>"
echo "<P ALIGN=left>&nbsp;&nbsp;"
echo "<img src=/SunINTFCal/cgi-bin/img/Sun2.png height="100" width="100%"></P>"
echo "<a target="_blank" href="/SunINTFCal/help.pdf" title="Report of the operative sun monitoring process" align="right"><font size=4>Documentation</font></a>" 
echo "<br>"
echo "<hr size=2 width=100% color="black" align= right />"
echo "<font color="black" face="Verdana" size="5">Last updated results</font>"
echo "<hr size=2 width=100% color="black" align= right />"

if [ -n "$calendar" ]
then
	echo "<font color="black" face="Verdana" size="4"> CALENDAR: Query for $radar</font>"
	echo "<hr size=2 width=100% color="black" align= right />"
	echo "<br>"

	figure=$WEBpathPLOTS/"NINTFcalendar_"$radar".png"

	echo "<table style="width:100%">"
	echo "<tr>"
	echo "<th style="width:2%"></th>"
	echo "<th style="width:96%"></th>"
	echo "<th style="width:2%"></th>"
	echo "</tr>"
	
	echo "<tr>"
	echo "<td align="center"></td>"

	if [ -f /var/www/html/$figure ]
		then
			echo "<td align="center"><img src=$figure alt="" height="200" width="95%" align="center"></img></td>"
		else
			echo "<td align="center"><font color="white" face="Verdana" size="4"> [Figure not available] </font></td>"
	fi
	echo "<td align="center"></td>"
	echo "</tr>"
	echo "</table>"

else
	echo "<font color="black" face="Verdana" size="4"> POLAR PLOTS: Query for $radar</font>"
	echo "<hr size=2 width=100% color="black" align= right />"
	echo "<br>"

	# Display selected plots in the html document
	for i in ${plots[@]}
	do
		
                figureALL=$WEBpathPOLAR/"Polar"$i"_ALL_"$radar".png"
		figureSOL=$WEBpathPOLAR/"Polar"$i"_SOLAR_"$radar".png"

		echo "<table style="width:100%">"
		echo "<tr>"
		echo "<th style="width:50%">ALL interferences</th>"
		echo "<th style="width:50%">SOLAR interferences</th>"
		echo "</tr>"
		echo "<tr>"

		if [ -f /var/www/html/$figureALL ]
		then
			echo "<td align="center"><img src=$figureALL alt="" height="200" width="100%"></img></td>"
		else
			echo "<td align="center"><font color="white" face="Verdana" size="4"> [Figure not available] </font></td>"
		fi

		if [ -f /var/www/html/$figureSOLAR ]
		then
			echo "<td align="center"><img src=$figure5P alt="" height="200" width="100%"></img></td>"
		else
			echo "<td align="center"><font color="white" face="Verdana" size="4"> [Figure not available] </font></td>"
		fi

                echo "</tr>"
		echo "</table>"

	done
fi

echo "<br><br><br><br><br><br><br><br><br><br>"

echo "<hr size=1 width=100% color="black" align= right />"
echo "<table style="width:100%"><tr><th style="width:50%" align="left"><img src=/SunINTFCal/cgi-bin/img/generalitat_logo.gif height="25" width="35%"></th>"
echo "<th style="width:50%" align="right"><img src=/SunINTFCal/cgi-bin/img/meteocat_logo.jpg height="25" width="13%"></th></tr></table>"
echo "<hr size=1 width=100% color="black" align= right />"

echo "</BODY></HTML>"






