capture log close
set more off
clear all


log using "C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Logs/Andrew/BD-dm-EE-enrol.log", text replace

*--------------------------------------------
* BD-EE-dm-enrol.do
*
* andrew mertens (amertens@berkeley.edu)
*
* Create the EED substudy set of enrollment 
* covariates
*
*--------------------------------------------




*--------------------------------------------
* input files:
*
*  Cleaned main trial enrollment dataset 
*    washb-bangladesh-enrol.dta
* 
*  Raw main trial baseline survey
*	 1. WASHB_Baseline_main_survey_cleaned.dta
*
*--------------------------------------------

clear
cd "C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Untouched"
use "1. WASHB_Baseline_main_survey.dta"
keep dataid q4114_1h q4114_2h q4114_3h
rename q4114_1h n_cows
rename q4114_2h n_goats
rename q4114_3h n_chickens
sort dataid

tempfile animals
save `animals'

use "washb-bangladesh-enrol.dta"
sort dataid
merge dataid using `animals'
tab _merge
drop _merge


cd "C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Temp"
save "washb-bangladesh-enrol+animals.dta", replace
outsheet using "washb-bangladesh-enrol+animals.csv", comma replace
clear
