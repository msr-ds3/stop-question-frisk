all:

sqf_2003.csv sqf_2004.csv sqf_2005.csv sqf_2006.csv sqf_2007.csv sqf_2008.csv sqf_2009.csv sqf_2010.csv sqf_2011.csv sqf_2012.csv sqf_2013.csv sqf_2014.csv sqf_2015.csv sqf_2016.csv sqf_2017.xlsx sqf_2018.xlsx: 01_download_sqf_data.sh
	./01_download_sqf_data.sh

: 02_stop_question_frisk_clean_data.R
	Rscript 02_stop_question_frisk_clean_data.R

: 05_stop_and_frisk_clean_data.R
	Rscript 05_stop_and_frisk_clean_data.R

: 08_sqf_summary_stats.R
	Rscript 08_sqf_summary_stats.R

: 10_a_sqf_logistic_regressions.R
	Rscript 10_a_sqf_logistic_regressions.R

: 10_b_sqf_AUC.R
	Rscript 10_b_sqf_AUC.R

PPCS_cleaning_files = 06A_clean_ppcs_1996.Rmd 06B_clean_ppcs_1999.Rmd 06C_clean_ppcs_2002.Rmd 06D_clean_ppcs_2005.Rmd 06E_clean_ppcs_2008.Rmd 06F_clean_ppcs_2011.Rmd 06G_clean_ppcs_2015.Rmd

: PPCS_cleaning_files
	for i in $(PPCS_cleaning_files); do\
		Rscript

clean: sqf_2003.csv sqf_2004.csv sqf_2005.csv sqf_2006.csv sqf_2007.csv sqf_2008.csv sqf_2009.csv sqf_2010.csv sqf_2011.csv sqf_2012.csv sqf_2013.csv sqf_2014.csv sqf_2015.csv sqf_2016.csv sqf_2017.xlsx sqf_2018.xlsx
