#!/bin/bash

echo "Content-type: text/html"
echo ""

IFS='&' # Change default whitespace splitting character for an ampersand
plots=()
COUNTER=1

bla="Rsq"

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


# Model inversion type choice
if [ $(echo "$mode" | grep -c "3day") -ne 0 ]
then

	fitstr="_3day"
	title="Model fit to 3-DAY observations "
else

	if [ $(echo "$mode" | grep -c "1day") -ne 0 ]
	then
		fitstr="_1day"
		title="Model fit to DAILY observations "
	else
		fitstr="xx"
	fi
fi

# Out path for web plots with respect to "/var/www/html"
WEBpathPLOTS="/SunINTFCal/$radar" 

# Write html
echo "<HTML>"
echo "<HEAD>"
echo "<title>SUN MONITORING</title>"
echo "</HEAD>"
echo "<BODY bgcolor="white">"
echo "<a name="TOP"></a>"
echo "<P ALIGN=left>&nbsp;&nbsp;"
echo "<img src=/SunINTFCal/cgi-bin/img/Sun2.png height="100" width="100%"></P>"
echo "<a target="_blank" href="/SunINTFCal/help.pdf" title="Help document on the operative process" align="right"><font size=4>help</font></a>" 
echo "<br>"
echo "<hr size=2 width=100% color="black" align= right />"
echo "<font color="black" face="Verdana" size="5">Last updated results</font>"
echo "<hr size=2 width=100% color="black" align= right />"
echo "<font color="black" face="Verdana" size="4"> CALIBRATION: Query for $radar, $title</font>"
echo "<hr size=2 width=100% color="black" align= right />"
echo "<br>"

# Display selected plots in the html document
for i in ${plots[@]}
do

	pl=$(printf "%s\n" $i)	

	if [ $(echo "$pl" | grep -c "Widths") -ne 0 ]
	then

		figure=$WEBpathPLOTS/$i"_5P"$fitstr"_"$radar".png"

		echo "<table style="width:100%">"
		echo "<tr>"
		echo "<th style="width:100%">Solar widths (only 5P-model)</th>"
		echo "</tr>"
		echo "<tr>"

		if [ -f /var/www/html/$figure ]
		then
			echo "<td align="center"><img src=$figure alt="" height="200" width="50%"></img></td>"
		else
			echo "<td align="center"><font color="white" face="Verdana" size="4"> [Figure not available] </font></td>"
		fi

	elif [ $(echo "$pl" | grep -c "Qfit") -ne 0 ]
	then

		figure3P=$WEBpathPLOTS/$i"_RMSE"$fitstr"_"$radar".png"
		figure5P=$WEBpathPLOTS/$i"_Rsq"$fitstr"_"$radar".png"

		echo "<table style="width:100%">"
		echo "<tr>"
		echo "<th style="width:50%">RMSE of fit residuals</th>"
		echo "<th style="width:50%">R-squared of fit </th>"
		echo "</tr>"
		echo "<tr>"

		if [ -f /var/www/html/$figure3P ]
		then
			echo "<td align="center"><img src=$figure3P alt="" height="200" width="100%"></img></td>"
		else
			echo "<td align="center"><font color="white" face="Verdana" size="4"> [Figure not available] </font></td>"
		fi

		if [ -f /var/www/html/$figure5P ]
		then
			echo "<td align="center"><img src=$figure5P alt="" height="200" width="100%"></img></td>"
		else
			echo "<td align="center"><font color="white" face="Verdana" size="4"> [Figure not available] </font></td>"
		fi
		
	else

		figure3P=$WEBpathPLOTS/$i"_3P"$fitstr"_"$radar".png"
		figure5P=$WEBpathPLOTS/$i"_5P"$fitstr"_"$radar".png"

		echo "<table style="width:100%">"
		echo "<tr>"
		echo "<th style="width:50%">$i (3P-model)</th>"
		echo "<th style="width:50%">$i (5P-model)</th>"
		echo "</tr>"
		echo "<tr>"

		if [ -f /var/www/html/$figure3P ]
		then
			echo "<td align="center"><img src=$figure3P alt="" height="200" width="100%"></img></td>"
		else
			echo "<td align="center"><font color="white" face="Verdana" size="4"> [Figure not available] </font></td>"
		fi

		if [ -f /var/www/html/$figure5P ]
		then
			echo "<td align="center"><img src=$figure5P alt="" height="200" width="100%"></img></td>"
		else
			echo "<td align="center"><font color="white" face="Verdana" size="4"> [Figure not available] </font></td>"
		fi

	fi

	echo "</tr>"
	echo "</table>"

done

echo "<br><br><br><br><br><br><br><br><br><br>"

echo "<hr size=1 width=100% color="black" align= right />"
echo "<table style="width:100%"><tr><th style="width:50%" align="left"><img src=/SunINTFCal/cgi-bin/img/generalitat_logo.gif height="25" width="35%"></th>"
echo "<th style="width:50%" align="right"><img src=/SunINTFCal/cgi-bin/img/meteocat_logo.jpg height="25" width="13%"></th></tr></table>"
echo "<hr size=1 width=100% color="black" align= right />"

echo "</BODY></HTML>"






