capture log close
set more off
clear all


log using "C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Logs/Andrew/BD-dm-EE-stool.log", text replace

*--------------------------------------------
* BD-EE-dm-medhistory.do
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
* Merge in Date of Births from the main study 
* anthro and diar datasets to use in child age 
* calculations
*--------------------------------------------
clear
cd "C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Untouched"


*Calculate child age at each sample collection
*First, merge in main study DOB from anthro and diar datasets
use washb-bangladesh-anthro, clear

duplicates tag dataid childid, generate(dup)
keep if childid=="T1" | childid=="T2"

gen childNo= substr(childid,2,1) //change childid to match EE
rename dob anthrodob
rename sex anthrosex
gen byte MainStudyDataset_anthro_DOB=1 
gen byte MainStudyDataset_anthro_sex=1 

keep dataid childNo svy anthrodob MainStudyDataset_anthro_DOB MainStudyDataset_anthro_sex anthrosex
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

keep dataid childNo svy diardob MainStudyDataset_diar_DOB MainStudyDataset_diar_sex diarsex
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
gen byte EE_svy=1 
gen byte EEsex=1 if q14==1
  replace EEsex=0 if q14==2
  label var EEsex "1=male, 0=female"
keep dataid childNo EEdob EE_svy EEsex 

gen EE_dob = date(EEdob, "DMY")
drop EEdob
format EE_dob %d

tempfile BL_EE_dob
save `BL_EE_dob'

    *Save relevent parts of midline childid tracking
use Midline/ChildID_Midline_Cleaned_MatchedwEnrollment_2Feb16, clear

rename q15 EEdob
rename childno childNo
gen byte EE_svy=2 
gen byte EEsex=1 if q14==1
  replace EEsex=0 if q14==2
  label var EEsex "1=male, 0=female"
keep dataid childNo EEdob EE_svy EEsex 

gen EE_dob = date(EEdob, "DMY")
drop EEdob
format EE_dob %d

tempfile ML_EE_dob
save `ML_EE_dob'
	
    *Save relevent parts of endline childid tracking
use Endline/EE_Endline_ChildID&MedHistory_CLEANED_data_22June2016, clear

rename q15 EEdob
gen byte EE_svy=3
gen byte EEsex=1 if q14==1
  replace EEsex=0 if q14==2
  label var EEsex "1=male, 0=female"
keep dataid childNo EEdob EE_svy EEsex

format EEdob %d
rename EEdob EE_dob

*Append medhistory survey year DOBs
append using `BL_EE_dob', force nolabel
append using `ML_EE_dob', force nolabel 


*Clean variables
sort dataid childNo EE_svy
label var EE_svy "Year of EE medhistory survey that the DOB was taken from"


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

tempfile DOB
save `DOB'



************************************
*Load and append medical history forms
************************************
use Midline/ChildID_Midline_Cleaned_MatchedwEnrollment_2Feb16, clear
gen byte svy=2 
gen EE_dob = date(q15, "DMY")
drop q15
format EE_dob %d
rename childno childNo
destring clusterid numchildren, replace

gen SampleColDate = date(samplecoldate, "DMY")
drop samplecoldate
format SampleColDate %d

tempfile ML_EE_dob
save `ML_EE_dob'



use Endline/EE_Endline_ChildID&MedHistory_CLEANED_data_22June2016, clear
gen byte svy=3 
rename q15 EE_dob
format EE_dob %d

gen samplecoldate = date(SampleColDate, "DMY")
drop SampleColDate
rename samplecoldate SampleColDate
format SampleColDate %d

rename NumChildren numchildren
destring numchildren, replace

tempfile EL_EE_dob
save `EL_EE_dob'


use Baseline/Baseline_ChildID_CLEANED_VersionIncorporated_20Oct15, clear
gen byte svy=1 
gen EE_dob = date(q15, "DMY")
drop q15
format EE_dob %d
destring clusterid numchildren, replace


gen SampleColDate = date(samplecoldate, "DMY")
drop samplecoldate
format SampleColDate %d
rename childno childNo

append using `ML_EE_dob', force
append using `EL_EE_dob', force


*Variable cleaning
rename q17 aged
rename q5 consent


*Temporarily limit variables in dataset to help with reshape
keep dataid clusterid consent SampleColDate numchildren childNo aged EE_dob svy 


	label var aged "Age in days (anthro meas)"
gen double agem = aged/30.4375
	label var agem "Age in months (anthro meas)"
gen double agey = aged/365.25
	label var agey "Age in years (anthro meas)"
codebook agey

label var SampleColDate "Date of sample collection"
* Month of measurement
gen month = month(SampleColDate)
	label var month "Month of sample collection"

	
*Investigate genders not consistent across follow up rounds
   *Impute missing genders 
   *replace sexEE=1 if dataid=="10502"
   *replace sexEE=0 if dataid=="03803" | dataid=="16906"
   

*Merge DOBs into medical history dataset
sort dataid childNo
merge m:1 dataid childNo using `DOB'
tab _merge
drop if _merge!=3

list dataid childNo svy if DOBfromEE==1
list dataid childNo svy if sexfromEE==1

drop _merge dup DOBfromEE sexfromEE

	
************************************
*Reshape to wide
************************************

*Reshape to wide
reshape wide consent SampleColDate agem agey aged numchildren EE_dob month, i(dataid childNo) j(svy)
*Check for any duplicates after reshaping
duplicates list dataid childNo 

*Save file 
label data "BD EE medical history dataset, created by BD-EE-dm-medhistory.do"
saveold "C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Cleaned/Andrew/BD-EE-medhistory.dta", replace version(12)
outsheet using "C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Cleaned/Andrew/BD-EE-medhistory.csv", comma replace
clear
