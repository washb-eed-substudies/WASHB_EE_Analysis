capture log close
set more off
clear all


log using "C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Logs/Andrew/BD-dm-EE-telo.log", text replace

*--------------------------------------------
* BD-EE-dm-telo.do
*
* andrew mertens (amertens@berkeley.edu)
*
* process the Bangladesh EED substudy time 1,2, and 3
* urine data for analysis
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
* Extract child birth order from the main study 
* anthro data
*--------------------------------------------
use "C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Untouched/washb-bangladesh-anthro.dta", clear
gen childNo= substr(childid,2,1)
duplicates drop dataid childNo, force  //*Only keep one round of data
keep dataid childNo birthord
sort dataid childNo	

tempfile birthorder
save `birthorder'


*--------------------------------------------
* Append 3 rounds of blood surveys and rename variables
*--------------------------------------------

use Endline/EE_Endline_Blood_CLEANED_data_26June2016, clear
rename q5 consent
rename FaId staffid
keep if consent=="1"
keep dataid childNo clusterid SampleColDate 
gen svy=3 //generate surveyyear variable

tempfile blood3
save `blood3'

use Midline/Blood_Midline_Cleaned_MatchedwEnrollment_2Feb16, clear
rename q5 consent
rename faid staffid
rename childno childNo
rename samplecoldate SampleColDate
destring clusterid, replace
keep if consent=="1"
keep dataid childNo clusterid SampleColDate
gen svy=2 //generate surveyyear variable

tempfile blood2
save `blood2'

use Baseline/Baseline_Blood_CLEANED_VersionIncorporated_30Sept14, clear
rename q5 consent
rename faid staffid
rename childno childNo
rename samplecoldate SampleColDate
destring clusterid, replace
keep if consent=="1"
keep dataid childNo clusterid SampleColDate 
gen svy=1 //generate surveyyear variable


*Append midline and endline centrifugation data
append using `blood2', force nolabel
append using `blood3', force nolabel

*Check for duplicate rows
sort dataid clusterid childNo  svy
duplicates tag dataid clusterid childNo svy , generate(dup)
list dataid clusterid childNo svy if dup>0
drop dup

sort dataid childNo svy

tempfile blood
save `blood'

*--------------------------------------------
* Append 3 rounds of blood centrifugation and rename variables
*--------------------------------------------

use Endline/EE_Endline_Blood_Centrifugation_24Jan17, clear
rename q22 sampid
rename q23 RandomID
rename q24 SampleType
rename q25 aliquot
generate str3 H01aliq= substr(sampid,-3,3)
keep if H01aliq=="H01" //Keep aliquots used in telomere analysis
keep dataid childno sampid aliquot RandomID
gen svy=3 //generate surveyyear variable

tempfile centri3
save `centri3'




use Midline/Blood_Midline_Centrifugation_24Jan17, clear
rename q22 sampid
rename q23 RandomID
rename q24 SampleType
rename q25 aliquot
generate str3 H01aliq= substr(sampid,-3,3)
keep if H01aliq=="H01" //Keep aliquots used in telomere analysis
keep dataid childno sampid aliquot RandomID
gen svy=2 //generate surveyyear variable

tempfile centri2
save `centri2'



use Baseline/Baseline_Blood_Centrifugation_24Jan17, clear
rename q22 sampid
rename q23 RandomID
rename q24 SampleType
rename q25 aliquot
generate str3 H01aliq= substr(sampid,-3,3)
keep if H01aliq=="H01" //Keep aliquots used in telomere analysis
keep dataid childno sampid aliquot RandomID
gen svy=1 //generate surveyyear variable



*Append midline and endline centrifugation data
append using `centri2', force nolabel
append using `centri3', force nolabel
sort dataid childno

*Check for duplicate rows
sort dataid childno sampid RandomID svy
duplicates tag dataid childno sampid RandomID svy , generate(dup)
list dataid childno sampid RandomID svy if dup>0
drop dup

rename childno childNo 

tempfile centri
save `centri'




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
	


*Merge DOBs into blood and centrifugation datasets
sort dataid childNo
merge 1:m dataid childNo using `centri'
tab _merge
drop if _merge==1
list dataid childNo svy if _merge==2
list dataid childNo svy if DOBfromEE==1
drop _merge 


sort dataid childNo svy
merge dataid childNo svy using `blood'
tab _merge
drop if _merge!=3 //Dropping many rows I need to figure out about
list dataid childNo svy if _merge==1 //Appear to be lab samples missing field data
drop _merge 


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
*Merge in birth order
************************************
sort dataid childNo	
merge dataid childNo using `birthorder'	
tab _merge
keep if _merge==3 | _merge==1
drop _merge


/*
*Temporarily generate variables
gen aged=0
gen agem=0
gen agey=0
gen month=0
gen birthorder=0
gen clusterid=0
gen date=0
*/
************************************
*Reshape to wide
************************************

*Temporarily limit variables in dataset to help with reshape
keep dataid clusterid svy sex DOB childNo aliquot sampid RandomID date aged agem agey month birthord

*Reshape to wide
reshape wide date aged agem agey month aliquot RandomID, i(dataid childNo sampid) j(svy)
*Check for any duplicates after reshaping
duplicates list dataid childNo 

*Save file 
label data "BD EE telomere dataset, created by BD-EE-dm-telo.do"
saveold "C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Cleaned/Andrew/BD-EE-telo.dta", replace version(12)
outsheet using "C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Cleaned/Andrew/BD-EE-telo.csv", comma replace
clear


















