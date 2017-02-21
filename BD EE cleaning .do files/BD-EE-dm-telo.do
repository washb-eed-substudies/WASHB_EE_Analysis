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
*Load in the outcome data
*--------------------------------------------
import excel using "C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Untouched/telomeres/WASH TL 07292016 (Midline Results).xlsx", sheet ("Sheet1") firstrow clear
replace TS="" if TS=="No amplification" 
destring TS, replace
format TS %16.0g
rename RandomID RandomID2_telo
rename sampleid sampid
rename TS TS2
gen dataid=substr(sampid,1,5)
gen childNo=substr(sampid,7,1)


keep dataid childNo TS2 sampid RandomID2_telo
sort dataid childNo
mean TS2
tempfile telo_lab_mid
save `telo_lab_mid'


import excel using "C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Untouched/telomeres/WASH TL 11112016 (Endline Results).xlsx", sheet ("Sheet1") firstrow clear
replace TS="" if TS=="DNA didn't pass QC" 
destring TS, replace
format TS %16.0g
rename RandomID RandomID3_telo
rename sampleid sampid
rename TS TS3
mean TS3

gen dataid=substr(sampid,1,5)
gen childNo=substr(sampid,7,1)



keep dataid childNo TS3 RandomID3_telo
sort dataid childNo
tempfile telo_lab_end
save `telo_lab_end'





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
cd "C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Untouched"
use Endline/EE_Endline_Blood_CLEANED_data_26June2016, clear
rename q5 consent
rename FaId staffid
keep dataid childNo clusterid SampleColDate staffid 
gen svy=3 //generate surveyyear variable

tempfile blood3
save `blood3'

use Midline/Blood_Midline_Cleaned_MatchedwEnrollment_2Feb16, clear
rename q5 consent
rename faid staffid
rename childno childNo
rename samplecoldate SampleColDate
destring clusterid, replace
keep dataid childNo clusterid SampleColDate staffid
gen svy=2 //generate surveyyear variable



tempfile blood2
save `blood2'

use Baseline/Baseline_Blood_CLEANED_VersionIncorporated_30Sept14, clear

rename q5 consent
rename faid staffid
rename childno childNo
rename samplecoldate SampleColDate
destring clusterid, replace
keep dataid childNo clusterid SampleColDate staffid
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
drop anthrodob diardob EE_dob MainStudyDataset_anthro_DOB MainStudyDataset_diar_DOB EE_dob1 EEsex1 EE_dob2 EEsex2 EE_dob3 EEsex3 EE_dob EEsex _merge svy diarsex anthrosex MainStudyDataset_diar_sex MainStudyDataset_anthro_sex DOBfromEE sexfromEE dup

sort dataid childNo
tempfile DOB
save `DOB'
clear


*-----------------
*Merge together centrifugation, 
*blood collection survey, and DOBs
*-----------------
use `centri'
sort dataid childNo svy
merge dataid childNo svy using `blood'
tab _merge
br if dataid=="04006" | RandomID=="3549259"
rename _merge centri_blood_merge

*Midline clusterid is missing from append, so apply endline clusterid to svy==2
bysort dataid: egen new_clusterid = mean(clusterid)
replace clusterid=new_clusterid
drop new_clusterid

*Merge DOBs into blood and centrifugation datasets
sort dataid childNo
merge dataid childNo using `DOB'
tab _merge
keep if _merge==3
drop _merge

/*
x
*Merge DOBs into blood and centrifugation datasets
sort dataid childNo
merge 1:m dataid childNo using `centri'
tab _merge
x
drop if _merge==1
list dataid childNo svy if _merge==2
list dataid childNo svy if DOBfromEE==1
drop _merge 


sort dataid childNo svy
merge dataid childNo svy using `blood'
tab _merge
br if dataid=="04006" | RandomID=="3549259"

list dataid clusterid childNo svy _merge if _merge==1 //List of lab samples missing field data
*drop if _merge!=3 //Dropping many rows I need to figure out about
drop _merge 
*/

************************************
*Clean variables
************************************
gen date = date(SampleColDate, "DMY")
drop SampleColDate
format date %d
	label var date "Date of sample collection"

rename aliquot aliquot_char
gen aliquot=1
	replace aliquot=2 if aliquot_char=="Partial Aliquot"
	replace aliquot=3 if aliquot_char=="Full Aliquot"



************************************
*Merge in birth order
************************************
sort dataid childNo	
merge dataid childNo using `birthorder'	
tab _merge
keep if _merge==3 | _merge==1
drop _merge


*Generate childid combining dataid and childNo
generate childid=dataid+childNo
sort childid svy

tempfile telo
save `telo'

************************************
*Merge in excel collection date, dob,
* and gender changes from lab staff
************************************
cd "C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Untouched/Changes"
import excel using washb-bd-ee-dob-changes-final.xlsx, sheet("Sheet1") firstrow clear
drop C
sort childid 
tempfile dob_change
save `dob_change'

import excel using washb-bd-ee-med-gender-changes-final.xlsx, sheet("Sheet2") firstrow clear
gen byte sex= Correctanswer=="Male"
keep dataid childNo sex
*rename Correctanswer sex
tostring childNo, replace
sort dataid childNo 
tempfile sex_change
save `sex_change'


import excel using washb-bd-ee-t1-blood-samplecoldate-changes-final.xlsx, sheet("Sheet1") firstrow clear
rename t1_blood date
gen svy=1
sort childid 
tempfile t1blood_change
save `t1blood_change'


import excel using washb-bd-ee-t2-blood-samplecoldate-changes-final.xlsx, sheet("Sheet1") firstrow clear
rename t2_blood date
gen svy=2
sort childid 
tempfile t2blood_change
save `t2blood_change'


clear 
use `telo' 
merge m:1 childid using `dob_change', keepusing(dob) update replace
tab _merge
drop _merge
merge m:1  dataid childNo using `sex_change', keepusing(sex) update replace
tab _merge
drop if _merge==2
drop _merge


codebook date
br if childid=="013041"
sort childid svy
merge m:1  childid svy using `t1blood_change', keepusing(date) update replace
tab _merge
drop _merge

merge m:1  childid svy using `t2blood_change', keepusing(date) update replace
tab _merge
drop _merge

codebook date


************************************
*Generate child ages
************************************
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
drop if svy==1 //Don't need baseline, and is interfering with reshape
keep dataid clusterid svy sex DOB childNo aliquot RandomID staffid date aged agem agey month birthord

*Reshape to wide
reshape wide date aged agem agey month aliquot RandomID staffid, i(dataid childNo) j(svy)
*Check for any duplicates after reshaping
duplicates list dataid childNo 
duplicates list dataid





************************************
*Merge in outcome data
************************************

sort dataid childNo
merge dataid childNo using `telo_lab_mid'
tab _merge
drop _merge

sort dataid childNo
merge dataid childNo using `telo_lab_end'
tab _merge
mean TS3

drop if _merge==2
mean TS3


*Only keep rows with outcome data
keep if TS2!=. | TS3!=.

*keep if TS2!=.
*list dataid childNo if agem2==.



*Generate clusterid for rows missing clusterid


drop clusterid
gen clusterid=substr(dataid,1,3)
destring clusterid, replace
sort clusterid
*br if clusterid==801

/*
tempfile telo
save `telo'

cd "C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Untouched/" 
use "washb-BD-telo-blind-tr.dta"

sort clusterid
merge clusterid using `telo'
tab _merge
list clusterid if _merge==2
*/

drop _merge
sort dataid
tempfile temp
save `temp'
clear

cd "C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Temp/"
use "washb-bangladesh-enrol+animals.dta"
sort dataid
merge dataid using `temp'
tab _merge
keep if _merge==3
mean TS2
mean watmin

*gen childid= dataid+childNo

*Save file 
label data "BD EE telomere dataset, created by BD-EE-dm-telo.do"
saveold "C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Cleaned/Andrew/BD-EE-telo.dta", replace version(12)
outsheet using "C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Cleaned/Andrew/BD-EE-telo.csv", comma replace
clear


















