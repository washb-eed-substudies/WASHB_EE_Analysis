capture log close
set more off
clear all


log using "C:/Users/andre/Dropbox/WBB-EE-analysis/Logs/Andrew/BD-dm-EE-stool.log", text replace

*--------------------------------------------
* 4-kenya-dm-stool.do
*
* andrew mertens (amertens@berkeley.edu)
*
* process the Bangladesh EED substudy time 1,2, and 3
* stool data for analysis
*
*--------------------------------------------




*--------------------------------------------
* input files:
*
*  Treatment assignments 
*    washb-bangladesh-blind-tr.dta
* 
*
*
*--------------------------------------------

*--------------------------------------------
* format the treatment assignment information
*--------------------------------------------
use "C:/Users/andre/Dropbox/WBB-EE-analysis/Data/Untouched/washb-bangladesh-blind-tr.dta", clear

destring clusterid, replace
sort clusterid
tempfile trdata
save `trdata'


*--------------------------------------------
* Append 3 rounds of stool collection surveys and rename variables
*--------------------------------------------
clear
cd "C:/Users/andre/Dropbox/WBB-EE-analysis/Data/Untouched"

use Endline/EE_Endline_Stool_CLEANED_data_7Sept2016, clear

rename q7s1 sampid1
rename q7s2 sampid2 
rename q7s3 sampid3
rename q7s4 sampid4
rename q7s5 sampid5
rename q8s1 randid1
rename q8s2 randid2 
rename q8s3 randid3
rename q8s4 randid4
rename q8s5 randid5
rename q11 nonconsent_reason 
rename FaId staffid

keep dataid clusterid mid staffid SampleColDate sampid1 sampid2 sampid3 sampid4 sampid5 randid1 randid2 randid3 randid4 randid5 nonconsent_reason 


*generate surveyyear variable
gen svy=3

tempfile c_stool3
save `c_stool3'

use Midline/Stool_Midline_Cleaned_MatchedwEnrollment_2Feb16, clear

rename q7s1 sampid1
rename q7s2 sampid2 
rename q7s3 sampid3
rename q7s4 sampid4
rename q7s5 sampid5
rename q8s1 randid1
rename q8s2 randid2 
rename q8s3 randid3
rename q8s4 randid4
rename q8s5 randid5
rename q11 nonconsent_reason 
rename faid staffid
rename samplecoldate SampleColDate

keep dataid clusterid mid staffid SampleColDate sampid1 sampid2 sampid3 sampid4 sampid5 randid1 randid2 randid3 randid4 randid5 nonconsent_reason 

*generate surveyyear variable
gen svy=2

tempfile c_stool2
save `c_stool2'

use Baseline/Baseline_Stool_CLEANED_VersionIncorporated_26Jan15, clear

rename q7s1 sampid1
rename q7s2 sampid2 
rename q7s3 sampid3
rename q7s4 sampid4
rename q7s5 sampid5
rename q8s1 randid1
rename q8s2 randid2 
rename q8s3 randid3
rename q8s4 randid4
rename q8s5 randid5
rename q11 nonconsent_reason 
rename faid staffid
rename samplecoldate SampleColDate

keep dataid clusterid mid staffid SampleColDate sampid1 sampid2 sampid3 sampid4 sampid5 randid1 randid2 randid3 randid4 randid5 nonconsent_reason 


*generate surveyyear variable
gen svy=1



*Append midline and endline anthropometry
append using `c_stool2', force nolabel
append using `c_stool3', force nolabel


*Save file 
label data "BD EE stool dataset, created by BD-EE-dm-stool.do"
saveold "C:/Users/andre/Dropbox/WBB-EE-analysis/Data/Final/BD-EE-stool.dta", replace version(12)
outsheet using "C:/Users/andre/Dropbox/WBB-EE-analysis/Data/Final/BD-EE-stool.csv", comma replace
clear
