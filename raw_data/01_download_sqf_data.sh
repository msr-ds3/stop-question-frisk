#!/bin/bash
#
# description:
#   fetches sqf files from the NYC Stop-Question and Frisk Data site https://www1.nyc.gov/site/nypd/stats/reports-analysis/stopfrisk.page
#
# usage: ./01_download_sqf_data.sh
#
# author:Roymil Terrero
#

# set a relative path for the NYPD sqf data
# (use current directory by default)
DATA_DIR=.

# change to the data directory
cd $DATA_DIR

# loop over each year {from 2003 to 2014}
for year in 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014
do
    # download the zip file
    # alternatively you can use wget if you don't have curl
    # wget $url
    url=https://www1.nyc.gov/assets/nypd/downloads/zip/analysis_and_planning/stop-question-frisk/sqf-${year}-csv.zip
    curl -O $url

    # define local file names
    file=`basename $url`
    name=-${year}-csv.zip
    csv=${file/$name/}"_${year}.csv"

    # unzip the downloaded file
    unzip -p $file > $csv

    # remove the zip file
    rm $file

done

# loop over year {from 2015 to 2016}
for year in 2015 2016
do
    # download the csv file
    url=https://www1.nyc.gov/assets/nypd/downloads/excel/analysis_and_planning/stop-question-frisk/sqf-${year}.csv
    curl -O $url

    # define local file names
    file=`basename $url`
    name=${file/-/_}
    mv $file $name
done

# loop over year {from 2017 to 2018}
for year in 2017 2018
do
    # download the xls file
    url=https://www1.nyc.gov/assets/nypd/downloads/excel/analysis_and_planning/stop-question-frisk/sqf-${year}.xlsx
    curl -O $url

    # define local file names
    file=`basename $url`
    name=${file/-/_}
    mv $file $name
done

