all: summarized_ppcs.RData summary_stats1.pdf sqf_03_18.RData model.rda log_data2.rds fryer_results.pdf our_result.pdf precinct_shape_file.RData census_race_data.RData

sqf_2003.csv sqf_2004.csv sqf_2005.csv sqf_2006.csv sqf_2007.csv sqf_2008.csv sqf_2009.csv sqf_2010.csv sqf_2011.csv sqf_2012.csv sqf_2013.csv sqf_2014.csv sqf_2015.csv sqf_2016.csv sqf_2017.xlsx sqf_2018.xlsx: 01_download_sqf_data.sh
	./01_download_sqf_data.sh

sqf_03_13.RData: 02_stop_question_frisk_clean_data.R sqf_2003.csv sqf_2004.csv sqf_2005.csv sqf_2006.csv sqf_2007.csv sqf_2008.csv sqf_2009.csv sqf_2010.csv sqf_2011.csv sqf_2012.csv sqf_2013.csv sqf_2014.csv sqf_2015.csv sqf_2016.csv sqf_2017.xlsx sqf_2018.xlsx
	Rscript 02_stop_question_frisk_clean_data.R

census_race_data.RData: 03_download_census_race_data.R
	Rscript 03_download_census_race_data.R

precinct_shape_file.RData: 04_download_precinct_shapefiles.R
	Rscript 04_download_precinct_shapefiles.R

sqf_03_18.RData: 05_stop_and_frisk_clean_data.R sqf_03_13.RData
	Rscript 05_stop_and_frisk_clean_data.R

ppcs_1996.RData: 06A_clean_ppcs_1996.Rmd 06999-0001-Data.txt 06999-0001-Setup.sas
	Rscript 06A_clean_ppcs_1996.Rmd

ppcs_1999.RData: 06B_clean_ppcs_1999.Rmd 03151-0001-Data.txt 03151-0001-Setup.sas
	Rscript 06B_clean_ppcs_1999.Rmd

ppcs_2002.RData: 06C_clean_ppcs_2002.Rmd 04273-0001-Data.txt 04273-0001-Setup.sas
	Rscript 06C_clean_ppcs_2002.Rmd

ppcs_2005.RData: 06D_clean_ppcs_2005.Rmd 20020-0001-Data.sav
	Rscript 06D_clean_ppcs_2005.Rmd

ppcs_2008.Rdata: 06E_clean_ppcs_2008.Rmd 32022-0001-Data.tsv
	Rscript 06E_clean_ppcs_2008.Rmd

ppcs_2011.RData: 06F_clean_ppcs_2011.Rmd 34276-0001-Data.rda
	Rscript 06F_clean_ppcs_2011.Rmd

ppcs_2015.RData: 06G_clean_ppcs_2015.Rmd ppcs_2015_raw.rda
	Rscript 06G_clean_ppcs_2015.Rmd

merged_ppcs.RData: 07_merge_ppcs.Rmd ppcs_1999.RData ppcs_1996.RData ppcs_2008.RData ppcs_2005.RData ppcs_2002.RData ppcs_2011.RData
	Rscript 07_merge_ppcs.Rmd

summary_stats1.pdf: 08_sqf_summary_stats.R sqf_03_13.RData
	Rscript 08_sqf_summary_stats.R

summarized_ppcs.RData: 09_ppcs_summary_stats.Rmd merged_ppcs.RData
	Rscript 09_ppcs_summary_stats.Rmd

model.rda log_data2.rds fryer_results.pdf our_result.pdf: 10_a_sqf_logistic_regressions.R sqf_03_13.RData
	Rscript 10_a_sqf_logistic_regressions.R

clean: sqf_2003.csv sqf_2004.csv sqf_2005.csv sqf_2006.csv sqf_2007.csv sqf_2008.csv sqf_2009.csv sqf_2010.csv sqf_2011.csv sqf_2012.csv sqf_2013.csv sqf_2014.csv sqf_2015.csv sqf_2016.csv sqf_2017.xlsx sqf_2018.xlsx sqf_03_13.RData sqf_03_18.RData summary_stats1.pdf model.rda log_data2.rds fryer_results.pdf our_result.pdf ppcs_1999.RData ppcs_1996.RData ppcs_2008.RData ppcs_2005.RData ppcs_2002.RData ppcs_2011.RData ppcs_2015.RData merged_ppcs.RData summarized_ppcs.RData census_race_data.RData precinct_shape_file.RData
