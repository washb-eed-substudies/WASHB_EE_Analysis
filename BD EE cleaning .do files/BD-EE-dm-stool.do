capture log close
set more off
clear all


log using "C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Logs/Andrew/BD-dm-EE-stool.log", text replace

*--------------------------------------------
* BD-EE-dm-stool.do
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
use "C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Untouched/washb-bangladesh-blind-tr.dta", clear

destring clusterid, replace
sort clusterid
tempfile trdata
save `trdata'


*--------------------------------------------
* Append 3 rounds of stool collection surveys and rename variables
*--------------------------------------------
clear
cd "C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Untouched"

use Endline/EE_Endline_Stool_CLEANED_data_7Sept2016, clear

rename q7s1 sampid1
rename q7s2 sampid2 
rename q7s3 sampid3
rename q7s4 sampid4
rename q7s5 sampid5
rename q7s6 sampid6
rename q8s1 randid1
rename q8s2 randid2 
rename q8s3 randid3
rename q8s4 randid4
rename q8s5 randid5
rename q8s6 randid6
rename q10s1 aliqout1_t
rename q10s2 aliqout2_t
rename q10s3 aliqout3_t
rename q10s4 aliqout4_t
rename q10s5 aliqout5_t
rename q10s6 aliqout6_t
rename q11 nonconsent_reason 
rename FaId staffid

destring clusterid, replace

keep dataid clusterid mid childNo staffid SampleColDate sampid1 sampid2 sampid3 sampid4 sampid5 sampid6 randid1 randid2 randid3 randid4 randid5 randid6 aliqout1_t aliqout2_t aliqout3_t aliqout4_t aliqout5_t aliqout6_t nonconsent_reason 


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
rename q10s1 aliqout1_t
rename q10s2 aliqout2_t
rename q10s3 aliqout3_t
rename q10s4 aliqout4_t
rename q10s5 aliqout5_t
rename q11 nonconsent_reason 
rename faid staffid
rename samplecoldate SampleColDate
rename childno childNo

destring clusterid, replace

keep dataid clusterid mid childNo staffid SampleColDate sampid1 sampid2 sampid3 sampid4 sampid5 randid1 randid2 randid3 randid4 randid5 aliqout1_t aliqout2_t aliqout3_t aliqout4_t aliqout5_t nonconsent_reason 

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
rename q10s1 aliqout1_t
rename q10s2 aliqout2_t
rename q10s3 aliqout3_t
rename q10s4 aliqout4_t
rename q10s5 aliqout5_t
rename q11 nonconsent_reason 
rename faid staffid
rename samplecoldate SampleColDate
rename childno childNo

destring clusterid, replace

keep dataid clusterid mid childNo staffid SampleColDate sampid1 sampid2 sampid3 sampid4 sampid5  randid1 randid2 randid3 randid4 randid5 aliqout1_t aliqout2_t aliqout3_t aliqout4_t aliqout5_t nonconsent_reason 


*generate surveyyear variable
gen svy=1



*Append midline and endline anthropometry
append using `c_stool2', force nolabel
append using `c_stool3', force nolabel
sort dataid childNo

*Appear to be fully duplicated observations (rows). List and drop here: 
duplicates tag dataid childNo sampid1 sampid2 sampid3 sampid4 sampid5 svy, generate(dup)
list dataid clusterid SampleColDate childNo sampid1 nonconsent_reason svy dup if dup>0
duplicates drop dataid childNo sampid1 sampid2 sampid3 sampid4 sampid5 svy, force

*Dataset cleaning
replace nonconsent_reason=0 if nonconsent_reason==.


tempfile stool
save `stool' 






********************************************************************************
*Calculate child age at each sample collection
********************************************************************************
*First, merge in main study DOB from anthro and diar datasets
use washb-bangladesh-anthro, clear

duplicates tag dataid childid, generate(dup)
keep if childid=="T1" | childid=="T2"

gen childNo= substr(childid,2,1) //change childid to match EE
rename dob anthrodob
rename sex anthrosex
gen byte MainStudyDataset_anthro_DOB=1 
gen byte MainStudyDataset_anthro_sex=1 
keep dataid childNo svy anthrodob MainStudyDataset_anthro_DOB anthrosex MainStudyDataset_anthro_sex
sort dataid childNo svy

duplicates drop dataid childNo anthrodob, force
duplicates list dataid childNo

tempfile anthro
save `anthro'

use washb-bangladesh-diar, clear
keep if childid=="T1" | childid=="T2"

gen childNo= substr(childid,2,1)
rename dob diardob
rename sex diarsex
gen byte MainStudyDataset_diar_DOB=1 
gen byte MainStudyDataset_diar_sex=1 
keep dataid childNo svy diardob MainStudyDataset_diar_DOB diarsex MainStudyDataset_diar_sex
sort dataid childNo svy

duplicates tag dataid childNo, generate(dup)
duplicates drop dataid childNo diardob, force
duplicates list dataid childNo

merge dataid childNo svy using `anthro' 
tab _merge
drop _merge dup
sort dataid childNo svy
duplicates list dataid childNo
duplicates tag dataid childNo, generate(dup)
drop if dup==1 & svy!=1

tempfile mainDOB
save `mainDOB' 






*Merge in the EE medical history files for the child date of birth for the 58 children
*not in the main study
    *Save relevent parts of baseline childid tracking
use Baseline/Baseline_ChildID_CLEANED_VersionIncorporated_20Oct15, clear
rename q15 EEdob
rename childno childNo
gen EE_svy=1
gen byte EEsex=1 if q14==1
  replace EEsex=0 if q14==2
  label var EEsex "1=male, 0=female"
keep dataid childNo EEdob EEsex EE_svy

gen EE_dob = date(EEdob, "DMY")
drop EEdob
format EE_dob %d

tempfile BL_EE_dob
save `BL_EE_dob'

    *Save relevent parts of midline childid tracking
use Midline/ChildID_Midline_Cleaned_MatchedwEnrollment_2Feb16, clear
rename q15 EEdob
rename childno childNo
gen EE_svy=2
gen byte EEsex=1 if q14==1
  replace EEsex=0 if q14==2
  label var EEsex "1=male, 0=female"
keep dataid childNo EEdob EEsex EE_svy

gen EE_dob = date(EEdob, "DMY")
drop EEdob
format EE_dob %d

tempfile ML_EE_dob
save `ML_EE_dob'
	
    *Save relevent parts of endline childid tracking
use Endline/EE_Endline_ChildID&MedHistory_CLEANED_data_22June2016, clear
rename q15 EEdob
gen EE_svy=3
gen byte EEsex=1 if q14==1
  replace EEsex=0 if q14==2
  label var EEsex "1=male, 0=female"
keep dataid childNo EEdob EEsex EE_svy

format EEdob %d
rename EEdob EE_dob

append using `BL_EE_dob', force nolabel
append using `ML_EE_dob', force nolabel 
sort dataid childNo



*Reshape to wide 
reshape wide EE_dob EEsex, i(dataid childNo) j(EE_svy)


*Only keep midline and endline DOBs not found at baseline
gen DOB=EE_dob1
	replace DOB=EE_dob2 if DOB==.
	replace DOB=EE_dob3 if DOB==.
format DOB %d


*Display mismatches between DOB across survey rounds
list dataid EE_dob1 EE_dob2 EE_dob3 DOB if EE_dob1!=EE_dob2 & EE_dob1!=. & EE_dob2!=. | EE_dob1!=EE_dob3 & EE_dob1!=. & EE_dob3!=.  | EE_dob2!=EE_dob3 & EE_dob2!=. & EE_dob3!=. 

gen sex=EEsex1
	replace sex=EEsex2 if sex==.
	replace sex=EEsex3 if sex==.

*Display mismatches between gender across survey rounds
list dataid EEsex1 EEsex2 EEsex3 sex if EEsex1!=EEsex2 & EEsex1!=. & EEsex2!=. | EEsex1!=EEsex3 & EEsex1!=. & EEsex3!=.  | EEsex2!=EEsex3 & EEsex2!=. & EEsex3!=. 

	
*Merge in childid tracking with main study DOBs
sort dataid childNo 
merge dataid childNo using `mainDOB' 
tab _merge

*list if dates mismatch between datasets
list dataid childNo svy anthrodob diardob if (MainStudyDataset_anthro_DOB==1 & MainStudyDataset_diar_DOB==1) & anthrodob!=diardob
list dataid childNo svy anthrodob DOB if (MainStudyDataset_anthro_DOB==1 & DOB!=.) & anthrodob!=DOB
list dataid childNo svy diardob DOB if (DOB!=. & MainStudyDataset_diar_DOB==1) & DOB!=diardob




*Look at two dataids without main study or EE BL DOBs and conflicting ML and EL DOBs
list dataid childNo DOB if dataid=="06103" | dataid=="29708" //Midline DOB correctly used

*Create single DOB variable, preferentially using main study DOB, then EEdob for the 37 subjects without a
*main study DOB
rename DOB EE_dob
generate DOB=anthrodob
replace DOB=diardob if MainStudyDataset_anthro_DOB!=1
replace DOB=EE_dob if MainStudyDataset_anthro_DOB!=1 & MainStudyDataset_diar_DOB!=1
format DOB %d

*Create indicator for whether dob comes from EE or main study
generate DOBfromEE byte=1 if EE_dob!=. & (MainStudyDataset_diar_DOB!=1 & MainStudyDataset_anthro_DOB!=1)
*Count how many DOB had to come from the EE surveys
table  DOBfromEE 

replace DOB=diardob if MainStudyDataset_anthro_DOB!=1
replace DOB=EE_dob if MainStudyDataset_anthro_DOB!=1 & MainStudyDataset_diar_DOB!=1
format DOB %d

*Create a single sex variable, preferentially using main study sex, then EEsex for the 30 subjects without a
*main study sex
rename sex EEsex
generate sex=anthrosex
replace sex=diarsex if MainStudyDataset_anthro_sex!=1
replace sex=EEsex if MainStudyDataset_anthro_sex!=1 & MainStudyDataset_diar_sex!=1
	label var sex "Male=1, Female=0"
	
*Create indicator for whether sex comes from EE or main study
generate sexfromEE byte=1 if EEsex!=. & (MainStudyDataset_diar_sex!=1 & MainStudyDataset_anthro_sex!=1)
*Count how many sex observations had to come from the EE surveys
table  sexfromEE 


*Drop unneeded variables
drop anthrodob diardob EE_dob MainStudyDataset_anthro_DOB MainStudyDataset_diar_DOB EE_dob1 EEsex1 EE_dob2 EEsex2 EE_dob3 EEsex3 EE_dob EEsex _merge svy diarsex anthrosex MainStudyDataset_diar_sex MainStudyDataset_anthro_sex
	


*Merge DOBs into urine dataset
sort dataid childNo
merge 1:m dataid childNo using `stool'
tab _merge
drop if _merge==1
list dataid childNo svy if _merge==2
list dataid childNo svy if DOBfromEE==1


************************************
*Generate child ages
************************************
gen date = date(SampleColDate, "DMY")
drop SampleColDate
format date %d
	label var date "Date of sample collection"
	
gen aged = date-DOB
	label var aged "Age in days (anthro meas)"
gen double agem = aged/30.4375
	label var agem "Age in months (anthro meas)"
gen double agey = aged/365.25
	label var agey "Age in years (anthro meas)"
codebook agey

* Month of measurement
gen month = month(date)
	label var month "Month of sample collection"

	
	
	

************************************
*Reshape to wide
************************************

*Temporarily limit variables in dataset to help with reshape
keep dataid clusterid svy sex DOB nonconsent_reason childNo aliqout1 aliqout2 aliqout3 aliqout4 aliqout5 date aged agem agey month 

*Reshape to wide
reshape wide date aged agem agey month  nonconsent_reason aliqout1 aliqout2 aliqout3 aliqout4 aliqout5, i(dataid childNo) j(svy)
*Check for any duplicates after reshaping
duplicates list dataid childNo 

*Save file 
label data "BD EE stool dataset, created by BD-EE-dm-stool.do"
saveold "C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Cleaned/Andrew/BD-EE-stool.dta", replace version(12)
outsheet using "C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Cleaned/Andrew/BD-EE-stool.csv", comma replace
clear


