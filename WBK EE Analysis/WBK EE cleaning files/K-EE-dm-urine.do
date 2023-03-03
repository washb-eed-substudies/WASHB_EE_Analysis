capture log close
set more off
clear all


log using "C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Logs/Andrew/BD-dm-EE-urine.log", text replace

*--------------------------------------------
* BD-EE-dm-urine.do
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
use "C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Untouched/washb-bangladesh-tr.dta", clear

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
* Append 3 rounds of stool collection surveys and rename variables
*--------------------------------------------
clear
cd "C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Untouched"


use Endline/EE_Endline_Urine_MainForm_CLEANED_data_11July2016, clear
rename FaId staffid
rename q7 consent
rename q8 nonconsent_reason 
rename q9 LMvol_t 
rename q31s1hour2 h2sampid1
rename q31s2hour2 h2sampid2
rename q31s3hour2 h2sampid3
rename q31s4hour2 h2sampid4
rename q31s5hour2 h2sampid5
rename q31s6hour2 h2sampid6
rename q31s7hour5 h5sampid7
rename q31s8hour5 h5sampid8
rename q31s9hour5 h5sampid9
rename q31s10hour5 h5sampid10
rename q31s11hour5 h5sampid11
rename q31s12hour5 h5sampid12
rename q34s1hour2 h2aliqout1_t 
rename q34s2hour2 h2aliqout2_t
rename q34s3hour2 h2aliqout3_t
rename q34s4hour2 h2aliqout4_t
rename q34s5hour2 h2aliqout5_t
rename q34s6hour2 h2aliqout6_t
rename q34s7hour5 h5aliqout7_t
rename q34s8hour5 h5aliqout8_t
rename q34s9hour5 h5aliqout9_t
rename q34s10hour5 h5aliqout10_t
rename q34s11hour5 h5aliqout11_t
rename q34s12hour5 h5aliqout12_t
rename q25 urineVol_t

       
*destring clusterid, replace
keep dataid childNo clusterid staffid SampleColDate NumChildren consent nonconsent_reason LMvol h2sampid1 h2sampid2 h2sampid3 h2sampid4 h2sampid5 h2sampid6 h5sampid7 h5sampid8 h5sampid9 h5sampid10 h5sampid11 h5sampid12 h2aliqout1_t h2aliqout2_t h2aliqout3_t h2aliqout4_t h2aliqout5_t h2aliqout6_t h5aliqout7_t h5aliqout8_t h5aliqout9_t h5aliqout10_t h5aliqout11_t h5aliqout12_t urineVol_t LMvol_t

*generate surveyyear variable
gen svy=3

tempfile endline
save `endline'


*Merge in preLM data to endline dataset 
use Endline/EE_Endline_PreLM_CLEANED_data_28July2016.dta, clear

rename q52s1hour2 preLMsampid13
rename q52s2hour2 preLMsampid14
rename q52s3hour2 preLMsampid15
rename q52s4hour2 preLMsampid16
rename q52s5hour2 preLMsampid17
rename q52s6hour2 preLMsampid18
rename q55s1hour2 preLMaliqout13_t
rename q55s2hour2 preLMaliqout14_t
rename q55s3hour2 preLMaliqout15_t
rename q55s4hour2 preLMaliqout16_t
rename q55s5hour2 preLMaliqout17_t
rename q55s6hour2 preLMaliqout18_t
rename q56hour2 preLMnonconsent_reason

destring preLMnonconsent_reason, replace

keep dataid childNo preLMsampid13 preLMsampid14 preLMsampid15 preLMsampid16 preLMsampid17 preLMsampid18 preLMaliqout13_t preLMaliqout14_t preLMaliqout15_t preLMaliqout16_t preLMaliqout17_t preLMaliqout18_t preLMnonconsent_reason 

merge 1:1 dataid childNo using `endline', force
tab _merge
drop _merge

tempfile c_urine3
save `c_urine3'


use Midline/Urine_Main_Midline_Cleaned_MatchedwEnrollment_2Feb16, clear
rename faid staffid
rename q7 consent
rename q8 nonconsent_reason 
rename q9 LMvol_t 
rename q31s1hour2 h2sampid1
rename q31s2hour2 h2sampid2
rename q31s3hour2 h2sampid3
rename q31s4hour2 h2sampid4
rename q31s5hour2 h2sampid5
rename q31s6hour2 h2sampid6
rename q31s7hour5 h5sampid7
rename q31s8hour5 h5sampid8
rename q31s9hour5 h5sampid9
rename q31s10hour5 h5sampid10
rename q31s11hour5 h5sampid11
rename q31s12hour5 h5sampid12
rename childno childNo
rename samplecoldate SampleColDate
rename numchildren NumChildren
rename q34s1hour2 h2aliqout1_t 
rename q34s2hour2 h2aliqout2_t
rename q34s3hour2 h2aliqout3_t
rename q34s4hour2 h2aliqout4_t
rename q34s5hour2 h2aliqout5_t
rename q34s6hour2 h2aliqout6_t
rename q34s7hour5 h5aliqout7_t
rename q34s8hour5 h5aliqout8_t
rename q34s9hour5 h5aliqout9_t
rename q34s10hour5 h5aliqout10_t
rename q34s11hour5 h5aliqout11_t
rename q34s12hour5 h5aliqout12_t
rename q25 urineVol_t

destring clusterid nonconsent_reason, replace

keep dataid childNo clusterid staffid SampleColDate NumChildren consent nonconsent_reason LMvol h2sampid1 h2sampid2 h2sampid3 h2sampid4 h2sampid5 h2sampid6 h5sampid7 h5sampid8 h5sampid9 h5sampid10 h5sampid11 h5sampid12 h2aliqout1_t h2aliqout2_t h2aliqout3_t h2aliqout4_t h2aliqout5_t h2aliqout6_t h5aliqout7_t h5aliqout8_t h5aliqout9_t h5aliqout10_t h5aliqout11_t h5aliqout12_t urineVol_t LMvol_t
 
*generate surveyyear variable
gen svy=2

tempfile midline
save `midline'

*Merge in preLM data to midline dataset 
use Midline/Pre-LM_Urine_Main_Midline_Cleaned_8Jan2017.dta, clear

rename q52s1hour2 preLMsampid13
rename q52s2hour2 preLMsampid14
rename q52s3hour2 preLMsampid15
rename q52s4hour2 preLMsampid16
rename q52s5hour2 preLMsampid17
rename q52s6hour2 preLMsampid18
rename q55s1hour2 preLMaliqout13_t
rename q55s2hour2 preLMaliqout14_t
rename q55s3hour2 preLMaliqout15_t
rename q55s4hour2 preLMaliqout16_t
rename q55s5hour2 preLMaliqout17_t
rename q55s6hour2 preLMaliqout18_t
rename q56hour2 preLMnonconsent_reason

destring preLMnonconsent_reason, replace

keep dataid childNo preLMsampid13 preLMsampid14 preLMsampid15 preLMsampid16 preLMsampid17 preLMsampid18 preLMaliqout13_t preLMaliqout14_t preLMaliqout15_t preLMaliqout16_t preLMaliqout17_t preLMaliqout18_t preLMnonconsent_reason

merge 1:1 dataid childNo using `midline', force
tab _merge
rename _merge midline_preLM_merge

tempfile c_urine2
save `c_urine2'


use Baseline/Baseline_Urine_CLEANED_VersionIncorporated_30Sept14, clear
rename faid staffid
rename q7 consent
rename q8 nonconsent_reason 
rename q9 LMvol_t 
rename q31s1hour2 h2sampid1
rename q31s2hour2 h2sampid2
rename q31s3hour2 h2sampid3
rename q31s4hour2 h2sampid4
rename q31s5hour2 h2sampid5
rename q31s6hour2 h2sampid6
rename q31s7hour5 h5sampid7
rename q31s8hour5 h5sampid8
rename q31s9hour5 h5sampid9
rename q31s10hour5 h5sampid10
rename q31s11hour5 h5sampid11
rename q31s12hour5 h5sampid12
rename childno childNo
rename samplecoldate SampleColDate
rename numchildren NumChildren
rename q34s1hour2 h2aliqout1_t 
rename q34s2hour2 h2aliqout2_t
rename q34s3hour2 h2aliqout3_t
rename q34s4hour2 h2aliqout4_t
rename q34s5hour2 h2aliqout5_t
rename q34s6hour2 h2aliqout6_t
rename q34s7hour5 h5aliqout7_t
rename q34s8hour5 h5aliqout8_t
rename q34s9hour5 h5aliqout9_t
rename q34s10hour5 h5aliqout10_t
rename q34s11hour5 h5aliqout11_t
rename q34s12hour5 h5aliqout12_t
rename q25 urineVol_t

destring clusterid, replace

keep dataid childNo clusterid staffid SampleColDate NumChildren consent nonconsent_reason LMvol h2sampid1 h2sampid2 h2sampid3 h2sampid4 h2sampid5 h2sampid6 h5sampid7 h5sampid8 h5sampid9 h5sampid10 h5sampid11 h5sampid12 h2aliqout1_t h2aliqout2_t h2aliqout3_t h2aliqout4_t h2aliqout5_t h2aliqout6_t h5aliqout7_t h5aliqout8_t h5aliqout9_t h5aliqout10_t h5aliqout11_t h5aliqout12_t urineVol_t LMvol_t

*generate surveyyear variable
gen svy=1


*Append midline and endline anthropometry
append using `c_urine2', force nolabel
append using `c_urine3', force nolabel
sort dataid childNo

*Dataset cleaning
replace consent=0 if consent==.
replace nonconsent_reason=0 if nonconsent_reason==.

replace SampleColDate="27 Jun 2013" if dataid=="17605" & svy==1 //Fix missing date from baseline to match enrollment records
replace SampleColDate="26 Feb 2013" if dataid=="09308" & svy==1 
replace SampleColDate="9 Jan 2013" if dataid=="02905" & svy==1 

tempfile urine
save `urine' 




********************************************************************************
*Calculate child age at each sample collection
********************************************************************************
*First, merge together main study DOB from anthro and diar datasets
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





********************************************************************************
*Merge in the EE medical history files for the child date of birth for the 58 children
*not in the main study
********************************************************************************
    *Save relevent parts of baseline childid tracking
use Baseline/Baseline_ChildID_CLEANED_VersionIncorporated_20Oct15, clear
rename q15 EEdob
rename childno childNo
gen EE_svy=1
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
gen EE_svy=2
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
gen EE_svy=3
gen byte EEsex=1 if q14==1
  replace EEsex=0 if q14==2
  label var EEsex "1=male, 0=female"
keep dataid childNo EEdob  EE_svy EEsex

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
*Generate diagnostic variable for inconistent sex
gen byte inconsist_sex= EEsex1!=EEsex2 & EEsex1!=. & EEsex2!=. | EEsex1!=EEsex3 & EEsex1!=. & EEsex3!=.  | EEsex2!=EEsex3 & EEsex2!=. & EEsex3!=.

	
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

*----------------
* new
*----------------


*30 children w/o main study anthro/diarrhea data, need to use EE sex and DOB (_merge==2)
replace dob = dob_fulldate3 if childid=="002081"
replace sex = sex_ee3 if childid=="002081"

replace dob = dob_fulldate1 if childid=="006021"
replace sex = sex_ee1 if childid=="006021"

replace dob = dob_fulldate2 if childid=="006041"
replace sex = sex_ee2 if childid=="006041"

replace dob = dob_fulldate1 if childid=="033061"
replace sex = sex_ee1 if childid=="033061"

replace dob = dob_fulldate2 if childid=="038011"
replace sex = sex_ee2 if childid=="038011"

*Shahjahan checked & correct dob was recorded at t2: 13dec2013
replace dob = dob_fulldate2 if childid=="061031"
replace sex = sex_ee2 if childid=="061031"

replace dob = dob_fulldate1 if childid=="065031"
replace sex = sex_ee1 if childid=="065031"

replace dob = dob_fulldate1 if childid=="076071"
replace sex = sex_ee1 if childid=="076071"

replace dob = dob_fulldate1 if childid=="093081"
replace sex = sex_ee1 if childid=="093081"

replace dob = dob_fulldate1 if childid=="101011"
replace sex = sex_ee1 if childid=="101011"

replace dob = dob_fulldate1 if childid=="103071"
replace sex = sex_ee1 if childid=="103071"

replace dob = dob_fulldate1 if childid=="117041"
replace sex = sex_ee1 if childid=="117041"
	
replace dob = dob_fulldate1 if childid=="121031"
replace sex = sex_ee1 if childid=="121031"

replace dob = dob_fulldate1 if childid=="128011"
replace sex = sex_ee1 if childid=="128011"

replace dob = dob_fulldate2 if childid=="153021"
replace sex = sex_ee2 if childid=="153021"

replace dob = dob_fulldate1 if childid=="156041"
replace sex = sex_ee1 if childid=="156041"

replace dob = dob_fulldate2 if childid=="201051"
replace sex = sex_ee2 if childid=="201051"

replace dob = dob_fulldate1 if childid=="204071"
replace sex = sex_ee1 if childid=="204071"

replace dob = dob_fulldate1 if childid=="222041"
replace sex = sex_ee1 if childid=="222041"

replace dob = dob_fulldate1 if childid=="234022"
replace sex = sex_ee1 if childid=="234022"

replace dob = dob_fulldate1 if childid=="268071"
replace sex = sex_ee1 if childid=="268071"

replace dob = dob_fulldate2 if childid=="271021"
replace sex = sex_ee2 if childid=="271021"

replace dob = dob_fulldate1 if childid=="271051"
replace sex = sex_ee1 if childid=="271051"

*Shahjahan checked & correct dob was recorded at t2: 15jun2013
replace dob = dob_fulldate2 if childid=="297081"
replace sex = sex_ee2 if childid=="297081"

replace dob = dob_fulldate1 if childid=="402071"
replace sex = sex_ee1 if childid=="402071"

replace dob = dob_fulldate1 if childid=="460031"
replace sex = sex_ee1 if childid=="460031"

replace dob = dob_fulldate1 if childid=="461071"
replace sex = sex_ee1 if childid=="461071"

replace dob = dob_fulldate1 if childid=="476011"
replace sex = sex_ee1 if childid=="476011"

replace dob = dob_fulldate1 if childid=="483011"
replace sex = sex_ee1 if childid=="483011"

replace dob = dob_fulldate2 if childid=="665051"
replace sex = sex_ee2 if childid=="665051"








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

*Look at where EE and main study genders do not agree
list dataid childNo EEsex EEsex1 EEsex2 EEsex3 diarsex anthrosex inconsist_sex if  (EEsex!=diarsex & diarsex!=. | EEsex!=anthrosex  & anthrosex!=.) & sexfromEE!=1 & inconsist_sex!=.

*List out sexes only in EE where sex isn't consistent over time
list EEsex diarsex anthrosex inconsist_sex if sexfromEE==1 & inconsist_sex==1

*Drop unneeded variables
drop anthrodob diardob EE_dob MainStudyDataset_anthro_DOB MainStudyDataset_diar_DOB EE_dob1 EEsex1 EE_dob2 EEsex2 EE_dob3 EEsex3 EE_dob EEsex _merge svy diarsex anthrosex MainStudyDataset_diar_sex MainStudyDataset_anthro_sex
	


*Merge DOBs into urine dataset
sort dataid childNo
merge 1:m dataid childNo using `urine'
tab _merge
drop if _merge==1
list dataid childNo svy if _merge==2
list dataid childNo svy if DOBfromEE==1
drop _merge

************************************
*Merge in birth order
************************************
sort dataid childNo	
merge dataid childNo using `birthorder'	
tab _merge
keep if _merge==3 | _merge==1
drop _merge

*generate date
gen date = date(SampleColDate, "DMY")
drop SampleColDate
format date %d
	label var date "Date of sample collection"


*Generate childid combining dataid and childNo
generate childid=dataid+childNo
sort childid svy

tempfile urine
save `urine'

************************************
*Merge in excel collection date, dob,
* and gender changes from lab staff
************************************
cd "C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Untouched/Changes"
import excel using washb-bd-ee-t1-urine-samplecoldate-changes-final.xlsx, sheet("Sheet1") firstrow clear
rename t1_urine date
gen svy=1
sort childid 
tempfile date_change1
save `date_change1'

import excel using washb-bd-ee-t2-urine-samplecoldate-changes-final.xlsx, sheet("Sheet1") firstrow clear
drop C
rename t2_urine date
gen svy=2
sort childid 
tempfile date_change2
save `date_change2'

import excel using washb-bd-ee-t3-urine-samplecoldate-changes-final.xlsx, sheet("Sheet1") firstrow clear
rename t3_urine date
drop C
gen svy=3
sort childid 
tempfile date_change3
save `date_change3'

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

clear 
use `urine' 
merge 1:1 childid svy using `date_change1', keepusing(date) update replace
tab _merge
drop _merge
merge 1:1 childid svy using `date_change2', keepusing(date) update replace
tab _merge
drop _merge
merge 1:1 childid svy using `date_change3', keepusing(date) update replace
tab _merge
drop _merge
merge m:1 childid using `dob_change', keepusing(dob) update replace
tab _merge
drop _merge
merge m:1  dataid childNo using `sex_change', keepusing(sex) update replace
tab _merge
drop if _merge==2
drop _merge





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

	

********************************************************************************
*Reshape to wide
********************************************************************************

*Temporarily limit variables in dataset to help with reshape
keep childid dataid clusterid svy consent nonconsent_reason staffid childNo DOB sex h2aliqout1_t h2aliqout2_t h2aliqout3_t h2aliqout4_t h2aliqout5_t h2aliqout6_t h5aliqout7_t h5aliqout8_t h5aliqout9_t h5aliqout10_t h5aliqout11_t h5aliqout12_t preLMaliqout13_t preLMaliqout14_t preLMaliqout15_t preLMaliqout16_t preLMaliqout17_t preLMaliqout18_t urineVol_t LMvol_t preLMnonconsent_reason aged agem agey date month birthord

*Reshape to wide
reshape wide consent nonconsent_reason staffid aged agem agey month date h2aliqout1_t h2aliqout2_t h2aliqout3_t h2aliqout4_t h2aliqout5_t h2aliqout6_t h5aliqout7_t h5aliqout8_t h5aliqout9_t h5aliqout10_t h5aliqout11_t h5aliqout12_t  preLMaliqout13_t preLMaliqout14_t preLMaliqout15_t preLMaliqout16_t preLMaliqout17_t preLMaliqout18_t urineVol_t preLMnonconsent_reason LMvol_t, i(dataid childNo) j(svy)
*Check for any duplicates after reshaping
duplicates list dataid childNo 


*Save file 
label data "BD EE urine dataset, created by BD-EE-dm-urine.do"
saveold "C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Cleaned/Andrew/BD-EE-urine.dta", replace version(12)
outsheet using "C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Cleaned/Andrew/BD-EE-urine.csv", comma replace
clear



