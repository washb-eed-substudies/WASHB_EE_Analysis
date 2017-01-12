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


use Endline/EE_Endline_Urine_MainForm_CLEANED_data_11July2016, clear
rename FaId staffid
rename q7 consent
rename q8 nonconsent_reason 
rename q9 LMvol
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
       
*destring clusterid, replace
keep dataid childNo clusterid staffid SampleColDate NumChildren consent nonconsent_reason LMvol h2sampid1 h2sampid2 h2sampid3 h2sampid4 h2sampid5 h2sampid6 h5sampid7 h5sampid8 h5sampid9 h5sampid10 h5sampid11 h5sampid12 h2aliqout1_t h2aliqout2_t h2aliqout3_t h2aliqout4_t h2aliqout5_t h2aliqout6_t h5aliqout7_t h5aliqout8_t h5aliqout9_t h5aliqout10_t h5aliqout11_t h5aliqout12_t

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
rename q9 LMvol
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

destring clusterid nonconsent_reason, replace

keep dataid childNo clusterid staffid SampleColDate NumChildren consent nonconsent_reason LMvol h2sampid1 h2sampid2 h2sampid3 h2sampid4 h2sampid5 h2sampid6 h5sampid7 h5sampid8 h5sampid9 h5sampid10 h5sampid11 h5sampid12 h2aliqout1_t h2aliqout2_t h2aliqout3_t h2aliqout4_t h2aliqout5_t h2aliqout6_t h5aliqout7_t h5aliqout8_t h5aliqout9_t h5aliqout10_t h5aliqout11_t h5aliqout12_t

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

x

use Baseline/Baseline_Urine_CLEANED_VersionIncorporated_30Sept14, clear
rename faid staffid
rename q7 consent
rename q8 nonconsent_reason 
rename q9 LMvol
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

destring clusterid, replace

keep dataid childNo clusterid staffid SampleColDate NumChildren consent nonconsent_reason LMvol h2sampid1 h2sampid2 h2sampid3 h2sampid4 h2sampid5 h2sampid6 h5sampid7 h5sampid8 h5sampid9 h5sampid10 h5sampid11 h5sampid12 h2aliqout1_t h2aliqout2_t h2aliqout3_t h2aliqout4_t h2aliqout5_t h2aliqout6_t h5aliqout7_t h5aliqout8_t h5aliqout9_t h5aliqout10_t h5aliqout11_t h5aliqout12_t 

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
gen byte MainStudyDataset_anthro_DOB=1 
keep dataid childNo svy anthrodob MainStudyDataset_anthro_DOB
sort dataid childNo svy

duplicates drop dataid childNo anthrodob, force
duplicates list dataid childNo

tempfile anthro
save `anthro'

use washb-bangladesh-diar, clear
keep if childid=="T1" | childid=="T2"

gen childNo= substr(childid,2,1)
rename dob diardob
gen byte MainStudyDataset_diar_DOB=1 
keep dataid childNo svy diardob MainStudyDataset_diar_DOB
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
gen byte EE_BLdataset_DOB=1 
gen female= q14-1
keep dataid childNo EEdob EE_BLdataset_DOB female

gen EE_dob = date(EEdob, "DMY")
drop EEdob
format EE_dob %d


tempfile BL_EE_dob
save `BL_EE_dob'

    *Save relevent parts of midline childid tracking
use Midline/ChildID_Midline_Cleaned_MatchedwEnrollment_2Feb16, clear
rename q15 EEdob
rename childno childNo
gen female= q14-1
gen byte EE_MLdataset_DOB=1 
keep dataid childNo EEdob EE_MLdataset_DOB female

gen EE_dob = date(EEdob, "DMY")
drop EEdob
format EE_dob %d

tempfile ML_EE_dob
save `ML_EE_dob'
	
    *Save relevent parts of endline childid tracking
use Endline/EE_Endline_ChildID&MedHistory_CLEANED_data_22June2016, clear
rename q15 EEdob
gen female= q14-1
gen byte EE_ELdataset_DOB=1 
keep dataid childNo EEdob EE_ELdataset_DOB female

format EEdob %d
rename EEdob EE_dob

append using `BL_EE_dob', force nolabel
append using `ML_EE_dob', force nolabel 
sort dataid childNo

*Only keep midline and endline DOBs not found at baseline
duplicates tag dataid childNo EE_dob ,generate(dup)
tab dup	
duplicates tag dataid childNo, generate(dup2)
tab dup2

*List out where DOB is not consistent between EE rounds
list dataid childNo EE_dob  EE_BLdataset_DOB EE_MLdataset_DOB EE_ELdataset_DOB  dup dup2 if dup!=dup2

*Keep preferentially baseline, then midline, then endline DOB where DOBs aren't consistent across rounds
drop if (EE_BLdataset_DOB!=1 & dup2==2) | (EE_ELdataset_DOB==1 & dup2==1) 
duplicates tag dataid childNo, generate(dup3)
drop if dup3==1 & EE_MLdataset_DOB==1

duplicates list dataid childNo
drop dup dup2 dup3 EE_BLdataset_DOB EE_MLdataset_DOB EE_ELdataset_DOB
gen byte EEdataset_DOB=1
	
	
*Merge in childid tracking with main study DOBs
sort dataid childNo 
merge dataid childNo using `mainDOB' 
tab _merge

*list if dates mismatch between datasets
list dataid childNo svy anthrodob diardob if (MainStudyDataset_anthro_DOB==1 & MainStudyDataset_diar_DOB==1) & anthrodob!=diardob
list dataid childNo svy anthrodob EE_dob if (MainStudyDataset_anthro_DOB==1 & EEdataset_DOB==1) & anthrodob!=EE_dob
list dataid childNo svy diardob EE_dob if (EEdataset_DOB==1 & MainStudyDataset_diar_DOB==1) & EE_dob!=diardob

*Create single DOB variable, preferentially using main study DOB, then EEdob for the 37 subjects without a
*main study DOB
generate DOB=anthrodob
replace DOB=diardob if MainStudyDataset_anthro_DOB!=1
replace DOB=EE_dob if MainStudyDataset_anthro_DOB!=1 & MainStudyDataset_diar_DOB!=1
format DOB %d

*Create indicator for whether dob comes from EE or main study
generate DOBfromEE byte=1 if EEdataset_DOB==1 & (MainStudyDataset_diar_DOB!=1 & MainStudyDataset_anthro_DOB!=1)
drop anthrodob diardob EE_dob MainStudyDataset_anthro_DOB MainStudyDataset_diar_DOB EEdataset_DOB _merge svy


*Merge DOBs into urine dataset
sort dataid childNo
merge 1:m dataid childNo using `urine'
tab _merge
drop if _merge==1
list dataid childNo svy if _merge==2
list dataid childNo svy if DOBfromEE==1




********************************************************************************
*Generate child ages
********************************************************************************
gen date = date(SampleColDate, "DMY")
drop SampleColDate
format date %d
	label var date "Date of sample collection"
	
gen aged = date-DOB
	label var aged "Age in days (anthro meas)"
gen double agem = aged/30.4167
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
keep dataid clusterid svy consent nonconsent_reason childNo DOB female h2aliqout1_t h2aliqout2_t h2aliqout3_t h2aliqout4_t h2aliqout5_t h2aliqout6_t h5aliqout7_t h5aliqout8_t h5aliqout9_t h5aliqout10_t h5aliqout11_t h5aliqout12_t preLMaliqout13_t preLMaliqout14_t preLMaliqout15_t preLMaliqout16_t preLMaliqout17_t preLMaliqout18_t preLMnonconsent_reason aged agem agey date month

*Reshape to wide
reshape wide consent nonconsent_reason female aged agem agey month date h2aliqout1_t h2aliqout2_t h2aliqout3_t h2aliqout4_t h2aliqout5_t h2aliqout6_t h5aliqout7_t h5aliqout8_t h5aliqout9_t h5aliqout10_t h5aliqout11_t h5aliqout12_t  preLMaliqout13_t preLMaliqout14_t preLMaliqout15_t preLMaliqout16_t preLMaliqout17_t preLMaliqout18_t preLMnonconsent_reason, i(dataid childNo) j(svy)
*Check for any duplicates after reshaping
duplicates list dataid childNo 


*Save file 
label data "BD EE urine dataset, created by BD-EE-dm-urine.do"
saveold "C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Cleaned/Andrew/BD-EE-urine.dta", replace version(12)
outsheet using "C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Cleaned/Andrew/BD-EE-urine.csv", comma replace
clear



