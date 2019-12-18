all:

02_stop_question_frisk_clean_data.R: 01_download_sqf_data.sh
	./01_download_sqf_data.sh

05_stop_and_frisk_clean_data.R: 02_stop_question_frisk_clean_data.R
	Rscript 02_stop_question_frisk_clean_data.R

08_sqf_summary_stats.R: 05_stop_and_frisk_clean_data.R
	Rscript 05_stop_and_frisk_clean_data.R



clean:
