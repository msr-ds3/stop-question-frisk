all: clean_data/summarized_ppcs.RData summary_stats1.pdf model.rda log_data2.rds fryer_results.pdf our_result.pdf 11_ppcs_regressions.html 10_c_sqf_tables.html figures/sqf_roc_curve_no_race.png figures/sqf_roc_curve.png figures/black_stop_rates_by_precinct.html figures/white_stop_rates_by_precinct.html 
# append the next line to the end of the previous line to include the webshots
# figures/white_stop_rates_by_precinct.png figures/black_stop_rates_by_precinct.png

raw_data/06999-0001-Data.txt raw_data/06999-0001-Setup.sas raw_data/03151-0001-Data.txt raw_data/03151-0001-Setup.sas raw_data/04273-0001-Data.txt raw_data/04273-0001-Setup.sas raw_data/20020-0001-Data.sav raw_data/32022-0001-Data.tsv raw_data/34276-0001-Data.rda raw_data/ppcs_2015.RData raw_data/36653-0001-Data.rda: 05_download_ppcs_data.sh
	./05_download_ppcs_data.sh
	date +"Current time is +%T"

raw_data/sqf_2003.csv raw_data/sqf_2004.csv raw_data/sqf_2005.csv raw_data/sqf_2006.csv raw_data/sqf_2007.csv raw_data/sqf_2008.csv raw_data/sqf_2009.csv raw_data/sqf_2010.csv raw_data/sqf_2011.csv raw_data/sqf_2012.csv raw_data/sqf_2013.csv raw_data/sqf_2014.csv raw_data/sqf_2015.csv raw_data/sqf_2016.csv raw_data/sqf_2017.xlsx raw_data/sqf_2018.xlsx: 01_download_sqf_data.sh
	./01_download_sqf_data.sh
	date +"Current time is +%T"

clean_data/sqf_03_13.RData: 02_stop_question_frisk_clean_data.R raw_data/sqf_2003.csv raw_data/sqf_2004.csv raw_data/sqf_2005.csv raw_data/sqf_2006.csv raw_data/sqf_2007.csv raw_data/sqf_2008.csv raw_data/sqf_2009.csv raw_data/sqf_2010.csv raw_data/sqf_2011.csv raw_data/sqf_2012.csv raw_data/sqf_2013.csv raw_data/sqf_2014.csv raw_data/sqf_2015.csv raw_data/sqf_2016.csv raw_data/sqf_2017.xlsx raw_data/sqf_2018.xlsx
	Rscript 02_stop_question_frisk_clean_data.R
	date +"Current time is +%T"

clean_data/census_race_data.RData: 03_download_census_race_data.R
	Rscript 03_download_census_race_data.R
	date +"Current time is +%T"

clean_data/precinct_shape_file.RData: 04_download_precinct_shapefiles.R
	Rscript 04_download_precinct_shapefiles.R
	date +"Current time is +%T"

clean_data/ppcs_1996.RData 06A_clean_ppcs_1996.html: 06A_clean_ppcs_1996.Rmd raw_data/06999-0001-Data.txt raw_data/06999-0001-Setup.sas
	Rscript -e 'rmarkdown::render("06A_clean_ppcs_1996.Rmd")'
	date +"Current time is +%T"

clean_data/ppcs_1999.RData 06B_clean_ppcs_1999.html: 06B_clean_ppcs_1999.Rmd raw_data/03151-0001-Data.txt raw_data/03151-0001-Setup.sas
	Rscript -e 'rmarkdown::render("06B_clean_ppcs_1999.Rmd")'
	date +"Current time is +%T"

clean_data/ppcs_2002.RData 06C_clean_ppcs_2002.html: 06C_clean_ppcs_2002.R raw_data/04273-0001-Data.txt raw_data/04273-0001-Setup.sas
	Rscript 06C_clean_ppcs_2002.R
	date +"Current time is +%T"

clean_data/ppcs_2005.RData 06D_clean_ppcs_2005.html: 06D_clean_ppcs_2005.Rmd raw_data/20020-0001-Data.sav
	Rscript -e 'rmarkdown::render("06D_clean_ppcs_2005.Rmd")'
	date +"Current time is +%T"

clean_data/ppcs_2008.RData 06E_clean_ppcs_2008.html: 06E_clean_ppcs_2008.Rmd raw_data/32022-0001-Data.tsv
	Rscript -e 'rmarkdown::render("06E_clean_ppcs_2008.Rmd")'
	date +"Current time is +%T"

clean_data/ppcs_2011.RData 06F_clean_ppcs_2011.html: 06F_clean_ppcs_2011.Rmd raw_data/34276-0001-Data.rda
	Rscript -e 'rmarkdown::render("06F_clean_ppcs_2011.Rmd")'
	date +"Current time is +%T"

clean_data/merged_ppcs.RData 07_merge_ppcs.html: 07_merge_ppcs.Rmd clean_data/ppcs_1999.RData clean_data/ppcs_1996.RData clean_data/ppcs_2008.RData clean_data/ppcs_2005.RData clean_data/ppcs_2002.RData clean_data/ppcs_2011.RData
	Rscript -e 'rmarkdown::render("07_merge_ppcs.Rmd")'
	date +"Current time is +%T"

summary_stats1.pdf: 08_sqf_summary_stats.R clean_data/sqf_03_13.RData
	Rscript 08_sqf_summary_stats.R
	date +"Current time is +%T"

clean_data/summarized_ppcs.RData 09_ppcs_summary_stats.html: 09_ppcs_summary_stats.Rmd clean_data/merged_ppcs.RData
	Rscript -e 'rmarkdown::render("09_ppcs_summary_stats.Rmd")'
	date +"Current time is +%T"

model.rda log_data2.rds figures/sqf_roc_curve_no_race.png figures/sqf_roc_curve.png: 10_a_sqf_logistic_regressions_new.R clean_data/sqf_03_13.RData
	Rscript 10_a_sqf_logistic_regressions_new.R
	date +"Current time is +%T"

#figures/sqf_roc_curve_no_race.png figures/sqf_roc_curve.png: 10_b_sqf_AUC.R clean_data/sqf_03_13.RData
#	Rscript 10_b_sqf_AUC.R

10_c_sqf_tables.html: 10_c_sqf_tables.Rmd log_data2.rds
	Rscript -e 'rmarkdown::render("10_c_sqf_tables.Rmd")'
	date +"Current time is +%T"

11_ppcs_regressions.html: 11_ppcs_regressions.Rmd clean_data/merged_ppcs.RData
	Rscript -e 'rmarkdown::render("11_ppcs_regressions.Rmd")'
	date +"Current time is +%T"

figures/black_stop_rates_by_precinct.html  figures/white_stop_rates_by_precinct.html: 12_map_per_capita_stop_rates_by_race.R clean_data/sqf_03_13.RData clean_data/census_race_data.RData clean_data/precinct_shape_file.RData
	Rscript 12_map_per_capita_stop_rates_by_race.R
	date +"Current time is +%T"

# uncomment out these lines to include the webshots
#figures/black_stop_rates_by_precinct.png figures/white_stop_rates_by_precinct.png: figures/black_stop_rates_by_precinct.html  figures/white_stop_rates_by_precinct.html 12b_map_per_capita_stop_rates_by_race.R
#	Rscript 12b_map_per_capita_stop_rates_by_race.R

clean:
	rm -r raw_data/sqf_2003.csv raw_data/sqf_2004.csv raw_data/sqf_2005.csv raw_data/sqf_2006.csv raw_data/sqf_2007.csv raw_data/sqf_2008.csv raw_data/sqf_2009.csv raw_data/sqf_2010.csv raw_data/sqf_2011.csv raw_data/sqf_2012.csv raw_data/sqf_2013.csv raw_data/sqf_2014.csv raw_data/sqf_2015.csv raw_data/sqf_2016.csv raw_data/sqf_2017.xlsx raw_data/sqf_2018.xlsx clean_data/sqf_03_13.RData model.rda log_data2.rds clean_data/ppcs_1999.RData clean_data/ppcs_1996.RData clean_data/ppcs_2008.RData clean_data/ppcs_2005.RData clean_data/ppcs_2002.RData clean_data/ppcs_2011.RData clean_data/merged_ppcs.RData clean_data/summarized_ppcs.RData clean_data/census_race_data.RData clean_data/precinct_shape_file.RData figures/black_stop_rates_by_precinct.html figures/black_stop_rates_by_precinct.png figures/white_stop_rates_by_precinct.html figures/white_stop_rates_by_precinct.png raw_data/06999-0001-Data.txt raw_data/06999-0001-Setup.sas raw_data/03151-0001-Data.txt raw_data/03151-0001-Setup.sas raw_data/04273-0001-Data.txt raw_data/04273-0001-Setup.sas raw_data/20020-0001-Data.sav raw_data/32022-0001-Data.tsv raw_data/34276-0001-Data.rda raw_data/36653-0001-Data.rda 11_ppcs_regressions.html 10_c_sqf_tables.html figures/sqf_roc_curve_no_race.png figures/sqf_roc_curve.png 
